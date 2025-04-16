library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(lubridate)
library(shinyBS) # For tooltips

# ---- Load Data ----
load_data <- function() {
  data <- list(
    elderly = read_csv("data/elderly.csv", show_col_types = FALSE,
                       col_types = cols(
                         elderly_id = col_character(),
                         name = col_character(),
                         age = col_integer(),
                         zipcode = col_character(),
                         location = col_character(),
                         contact_number = col_character(),
                         preferred_time_slots = col_character(),
                         major_city = col_character(),
                         state = col_character(),
                         lat = col_double(),
                         lng = col_double()
                       )),
    volunteers = read_csv("data/volunteers.csv", show_col_types = FALSE,
                          col_types = cols(
                            volunteer_id = col_character(),
                            name = col_character(),
                            zipcode = col_character(),
                            location = col_character(),
                            contact_number = col_character(),
                            availability = col_character(),
                            radius_km = col_double(),
                            radius_willingness = col_double(),
                            major_city = col_character(),
                            state = col_character(),
                            lat = col_double(),
                            lng = col_double(),
                            response_rate = col_double(),
                            skills = col_character()
                          )),
    care_requests = read_csv("data/care_requests.csv", show_col_types = FALSE,
                             col_types = cols(
                               id = col_character(),
                               elderly_id = col_character(),
                               needs = col_character(),
                               time_prefs = col_character(),
                               urgency = col_character()
                             )) %>%
      filter(!is.na(id)),
    elderly_requests = read_csv("data/elderly_requests.csv", show_col_types = FALSE,
                                col_types = cols(
                                  id = col_character(),
                                  request_id = col_character(),
                                  elderly_id = col_character(),
                                  requested_volunteers = col_character(),
                                  status = col_character(),
                                  modified_at = col_datetime()
                                )),
    volunteer_responses = read_csv("data/volunteer_responses.csv", show_col_types = FALSE),
    availability = read_csv("data/availability.csv", show_col_types = FALSE),
    needs = read_csv("data/needs.csv", show_col_types = FALSE),
    skills = read_csv("data/skills.csv", show_col_types = FALSE),
    elderly_needs = read_csv("data/elderly_needs.csv", show_col_types = FALSE),
    volunteer_skills = read_csv("data/volunteer_skills.csv", show_col_types = FALSE),
    carematch_predictions = read_csv("data/carematch_predictions.csv", show_col_types = FALSE,
                                     col_types = cols(
                                       request_id = col_character(),
                                       volunteer_id = col_character(),
                                       match_prob = col_double(),
                                       distance_km = col_double(),
                                       skill_match = col_double(),
                                       response_rate = col_double(),
                                       time_match = col_double()
                                     )) %>%
      mutate(
        request_id = str_trim(request_id),
        volunteer_id = str_trim(volunteer_id)
      ) %>%
      filter(!is.na(request_id), !is.na(volunteer_id)),
    historical_matches = read_csv("data/historical_matches.csv", show_col_types = FALSE)
  )
  
  # Extract unique zipcodes and their associated data
  all_zipcodes <- bind_rows(
    data$elderly %>% select(zipcode, major_city, state, lat, lng),
    data$volunteers %>% select(zipcode, major_city, state, lat, lng)
  ) %>%
    distinct(zipcode, .keep_all = TRUE)
  
  data$zipcode_data <- all_zipcodes
  data$unique_zipcodes <- setNames(all_zipcodes$zipcode, all_zipcodes$zipcode)
  
  # Extract unique time slots
  data$time_slots <- unique(data$availability$time_slot)
  
  return(data)
}

data <- load_data()

# Initialize reactive values for chat history
user_chat_history <- reactiveValues(
  elderly = list(),
  volunteer = list()
)

get_timestamp <- function() {
  format(Sys.time(), "%H:%M")
}

# ---- UI Definition ----
ui <- dashboardPage(
  dashboardHeader(title = "CareMatch"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Elderly Portal", tabName = "elderly", icon = icon("user")),
      menuItem("Volunteer Portal", tabName = "volunteer", icon = icon("hands-helping")),
      menuItem("Manage Elderly Profiles", tabName = "manage_elderly", icon = icon("user-cog")),
      menuItem("Manage Volunteer Profiles", tabName = "manage_volunteer", icon = icon("hands-helping"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$script(HTML("
        $(document).on('click', '.offer-btn', function() {
          Shiny.setInputValue('view_offers_request', this.id.replace('offer_', ''));
        });
        $(document).on('click', '.cancel-btn', function() {
          Shiny.setInputValue('cancel_request_id', this.id.replace('cancel_', ''));
        });
      "))
    ),
    tabItems(
      # Home Tab
      tabItem(
        tabName = "home",
        fluidRow(
          box(
            width = 12,
            h1("Welcome to CareMatch!"),
            p("Connecting elderly individuals with volunteers for caregiving."),
            actionButton("login_elderly", "Elderly Login", class = "btn-primary"),
            actionButton("login_volunteer", "Volunteer Login", class = "btn-success")
          )
        )
      ),
      # Elderly Portal
      tabItem(
        tabName = "elderly",
        fluidRow(
          box(
            width = 12,
            textInput("elderly_id", "Enter Elderly ID"),
            actionButton("elderly_login", "Login")
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Profile", uiOutput("elderly_profile")),
            tabPanel("My Active Requests", DTOutput("elderly_requests")),
            tabPanel("History", DTOutput("elderly_history")),
            tabPanel(
              "Chatbot",
              uiOutput("elderly_chatbot_ui")
            )
          )
        )
      ),
      # Volunteer Portal
      tabItem(
        tabName = "volunteer",
        fluidRow(
          box(
            width = 12,
            textInput("volunteer_id", "Enter Volunteer ID"),
            actionButton("volunteer_login", "Login")
          )
        ),
        fluidRow(
          tabBox(
            width = 12,
            tabPanel("Profile", uiOutput("volunteer_profile")),
            tabPanel("Inbox", DTOutput("volunteer_inbox")),
            tabPanel("Sent Offers", DTOutput("volunteer_sent")),
            tabPanel(
              "Chatbot",
              uiOutput("volunteer_chatbot_ui")
            )
          )
        )
      ),
      # Manage Elderly Profiles
      tabItem(
        tabName = "manage_elderly",
        fluidRow(
          box(
            width = 12,
            title = "Manage Elderly Profiles",
            selectInput("manage_elderly_id", "Select elderly profile:",
                        choices = c("New user..." = "new", setNames(data$elderly$elderly_id,
                                                                    paste(data$elderly$name, " (ID:", data$elderly$elderly_id, ")"))))
          )
        ),
        uiOutput("manage_elderly_ui")
      ),
      # Manage Volunteer Profiles
      tabItem(
        tabName = "manage_volunteer",
        fluidRow(
          box(
            width = 12,
            title = "Manage Volunteer Profiles",
            selectInput("manage_volunteer_id", "Select volunteer profile:",
                        choices = c("New volunteer..." = "new", setNames(data$volunteers$volunteer_id,
                                                                         paste(data$volunteers$name, " (ID:", data$volunteers$volunteer_id, ")"))))
          )
        ),
        uiOutput("manage_volunteer_ui")
      )
    )
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  user <- reactiveValues(
    type = NULL,
    id = NULL,
    data = NULL,
    step = NULL
  )
  
  rv <- reactiveValues(
    data = data,
    elderly_profile = NULL,
    volunteer_profile = NULL
  )
  
  # ---- Login Navigation ----
  observeEvent(input$login_elderly, {
    updateTabItems(session, "tabs", "elderly")
  })
  
  observeEvent(input$login_volunteer, {
    updateTabItems(session, "tabs", "volunteer")
  })
  
  # ---- Elderly Login ----
  observeEvent(input$elderly_login, {
    if (input$elderly_id %in% rv$data$elderly$elderly_id) {
      user$type <- "elderly"
      user$id <- input$elderly_id
      user$data <- rv$data$elderly %>% filter(elderly_id == input$elderly_id)
      showNotification("Elderly login successful", type = "message")
    } else {
      user$type <- NULL
      user$id <- NULL
      user$data <- NULL
      showNotification("Invalid ID", type = "error")
    }
  })
  
  # ---- Volunteer Login ----
  observeEvent(input$volunteer_login, {
    if (input$volunteer_id %in% rv$data$volunteers$volunteer_id) {
      user$type <- "volunteer"
      user$id <- input$volunteer_id
      user$data <- rv$data$volunteers %>% filter(volunteer_id == input$volunteer_id)
      showNotification("Volunteer login successful", type = "message")
    } else {
      user$type <- NULL
      user$id <- NULL
      user$data <- NULL
      showNotification("Invalid ID", type = "error")
    }
  })
  
  # ---- Elderly Portal Components ----
  output$elderly_profile <- renderUI({
    req(user$type == "elderly")
    u <- user$data
    tagList(
      h3(u$name),
      p(strong("Location:"), u$location %||% u$zipcode),
      p(strong("Contact:"), u$contact_number %||% "Not provided"),
      p(strong("Age:"), u$age %||% "Not provided")
    )
  })
  
  output$elderly_requests <- renderDataTable({
    req(user$type == "elderly")
    pending <- rv$data$care_requests %>%
      filter(elderly_id == user$id) %>%
      left_join(rv$data$elderly_requests, by = c("id" = "request_id")) %>%
      filter(status == "pending") %>%
      mutate(
        Actions = paste0(
          '<button id="offer_', id, '" class="btn btn-info btn-sm offer-btn">Offers</button>',
          ' <button id="cancel_', id, '" class="btn btn-danger btn-sm cancel-btn">Cancel</button>'
        )
      )
    datatable(
      pending %>% select(id, needs, time_prefs, urgency, status, Actions),
      escape = FALSE,
      selection = 'none',
      options = list(dom = 't')
    )
  })
  
  output$elderly_history <- renderDT({
    req(user$type == "elderly")
    rv$data$elderly_requests %>%
      filter(elderly_id == user$id, status == "completed") %>%
      left_join(rv$data$care_requests, by = c("request_id" = "id")) %>%
      select(request_id, needs, time_prefs, urgency, modified_at)
  })
  
  # ---- Volunteer Portal Components ----
  output$volunteer_profile <- renderUI({
    req(user$type == "volunteer")
    u <- user$data
    tagList(
      h3(u$name),
      p(strong("Location:"), u$location %||% u$zipcode),
      p(strong("Contact:"), u$contact_number %||% "Not provided"),
      p(strong("Skills:"), u$skills %||% "Not provided"),
      p(strong("Response Rate:"), paste0((u$response_rate %||% 0) * 100, "%")),
      p(strong("Radius:"), paste(u$radius_km %||% u$radius_willingness, "km"))
    )
  })
  
  output$volunteer_inbox <- renderDT({
    req(user$type == "volunteer")
    rv$data$volunteer_responses %>%
      filter(volunteer_id == user$id) %>%
      left_join(rv$data$care_requests, by = c("request_id" = "id")) %>%
      left_join(rv$data$elderly, by = c("elderly_id" = "elderly_id")) %>%
      select(request_id, elderly_name = name, needs, time_prefs, urgency, status)
  })
  
  output$volunteer_sent <- renderDT({
    req(user$type == "volunteer")
    rv$data$volunteer_responses %>%
      filter(volunteer_id == user$id, status == "accepted") %>%
      select(request_id, responded_at, status) %>%
      mutate(
        Cancel = paste0('<button class="btn btn-danger btn-sm">Cancel</button>')
      ) %>%
      datatable(escape = FALSE, selection = "none", options = list(dom = 'tp'))
  })
  
  # ---- Request Handling ----
  observeEvent(input$view_offers_request, {
    req_id <- input$view_offers_request
    responses <- rv$data$volunteer_responses %>%
      filter(request_id == req_id, status == "accepted") %>%
      left_join(rv$data$volunteers, by = c("volunteer_id" = "volunteer_id"))
    if (nrow(responses) == 0) {
      showModal(modalDialog("No offers yet. Please check back later."))
      return()
    }
    showModal(
      modalDialog(
        title = paste("Offers for Request", req_id),
        size = "l",
        footer = modalButton("Close"),
        easyClose = TRUE,
        tagList(
          lapply(1:nrow(responses), function(i) {
            row <- responses[i, ]
            actionButton(
              paste0("accept_vol_", row$volunteer_id),
              paste("Accept Offer from", row$name),
              class = "btn-success btn-sm"
            )
          })
        )
      )
    )
  })
  
  observeEvent(input$cancel_request_id, {
    req_id <- input$cancel_request_id
    rv$data$elderly_requests <- rv$data$elderly_requests %>%
      filter(request_id != req_id)
    write_csv(rv$data$elderly_requests, "data/elderly_requests.csv")
    showNotification("Request cancelled", type = "message")
  })
  
  observe({
    lapply(rv$data$volunteer_responses$volunteer_id, function(vol_id) {
      observeEvent(input[[paste0("accept_vol_", vol_id)]], {
        showNotification(paste("You accepted", vol_id, "! Match confirmed."), type = "message")
        rv$data$elderly_requests <- rv$data$elderly_requests %>%
          mutate(
            status = if_else(request_id == input$view_offers_request, "completed", status),
            modified_at = if_else(request_id == input$view_offers_request, Sys.time(), modified_at)
          )
        write_csv(rv$data$elderly_requests, "data/elderly_requests.csv")
        removeModal()
      })
    })
  })
  
  # ---- Elderly Chatbot Flow ----
  output$elderly_chatbot_ui <- renderUI({
    if (is.null(user$type) || user$type != "elderly" || is.null(user$id)) {
      tagList(
        p("Please log in with a valid Elderly ID to use the chatbot.")
      )
    } else {
      tagList(
        actionButton("start_elderly_chatbot", "Start Chatbot 💬", class = "btn-warning"),
        uiOutput("elderly_chatbot_flow")
      )
    }
  })
  
  observeEvent(input$start_elderly_chatbot, {
    user$step <- "start"
    user_chat_history$elderly <- list(
      div(
        style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
        strong("🤖 CareMatch Bot:"), " Welcome ", user$data$name %||% "User", "! How can I help?",
        tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
      )
    )
    output$elderly_chatbot_flow <- renderUI({
      tagList(
        user_chat_history$elderly,
        radioButtons(
          "elderly_choice",
          "Options:",
          choices = c(
            "(A) Add request" = "A",
            "(R) Recommend volunteer" = "R"
          )
        ),
        actionButton("elderly_next", "Send")
      )
    })
  })
  
  observeEvent(input$elderly_next, {
    req(input$elderly_choice)
    choice <- input$elderly_choice
    user_chat_history$elderly <- append(
      user_chat_history$elderly,
      list(
        div(
          style = "background-color:#dcf8c6; padding:10px; border-radius:10px; margin-left:40px;",
          strong("🧓 You:"), paste(choice),
          tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
        )
      )
    )
    if (choice == "A") {
      output$elderly_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$elderly,
          selectizeInput(
            "need_type",
            "Need Type:",
            choices = rv$data$needs$need_name,
            multiple = FALSE,
            options = list(placeholder = "e.g., Meal Preparation")
          ),
          selectizeInput(
            "time_slot",
            "Time Slot:",
            choices = rv$data$time_slots,
            multiple = TRUE,
            options = list(placeholder = "e.g., Weekday AM")
          ),
          selectInput(
            "urgency",
            "Urgency:",
            choices = c("Low", "Medium", "High")
          ),
          actionButton("submit_request", "Submit Request")
        )
      })
    } else if (choice == "R") {
      latest_request <- rv$data$elderly_requests %>%
        filter(elderly_id == user$id, status == "pending") %>%
        arrange(desc(modified_at)) %>%
        slice_head(n = 1)
      
      if (nrow(latest_request) == 0) {
        user_chat_history$elderly <- append(
          user_chat_history$elderly,
          list(
            div(
              style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
              strong("🤖 CareMatch Bot:"), " No pending requests found. Please add a request first using (A).",
              tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
            )
          )
        )
      } else {
        top_match <- rv$data$carematch_predictions %>%
          filter(request_id == latest_request$request_id)
        
        if (nrow(top_match) == 0) {
          user_chat_history$elderly <- append(
            user_chat_history$elderly,
            list(
              div(
                style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
                strong("🤖 CareMatch Bot:"), " No matches found for request ", latest_request$request_id, ". Please ensure the ML model (ml_training.R) has been run to generate predictions.",
                tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
              )
            )
          )
        } else {
          top_match <- top_match %>%
            arrange(desc(match_prob)) %>%
            slice_head(n = 1)
          
          user_chat_history$elderly <- append(
            user_chat_history$elderly,
            list(
              div(
                style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
                strong("🤖 CareMatch Bot:"), " Recommended Volunteer for Request ", top_match$request_id, ":",
                br(),
                "Volunteer ID: ", top_match$volunteer_id, br(),
                "Match Probability: ", sprintf("%.2f%%", top_match$match_prob * 100), br(),
                "Distance: ", sprintf("%.2f km", top_match$distance_km), br(),
                "Skill Match: ", sprintf("%.2f%%", top_match$skill_match * 100), br(),
                "Response Rate: ", sprintf("%.2f%%", top_match$response_rate * 100), br(),
                "Time Match: ", sprintf("%.2f%%", top_match$time_match * 100),
                tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
              )
            )
          )
        }
      }
      output$elderly_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$elderly,
          actionButton("start_elderly_chatbot", "Restart Chat")
        )
      })
    }
  })
  
  observeEvent(input$submit_request, {
    req(input$need_type, input$time_slot)
    new_id <- paste0("REQ_", nrow(rv$data$care_requests) + 1)
    rv$data$care_requests <- bind_rows(
      rv$data$care_requests,
      tibble(
        id = new_id,
        elderly_id = user$id,
        needs = input$need_type,
        time_prefs = paste(input$time_slot, collapse = ", "),
        urgency = input$urgency
      )
    )
    rv$data$elderly_requests <- bind_rows(
      rv$data$elderly_requests,
      tibble(
        id = paste0("ER_", nrow(rv$data$elderly_requests) + 1),
        request_id = new_id,
        elderly_id = user$id,
        requested_volunteers = "",
        status = "pending",
        modified_at = Sys.time()
      )
    )
    write_csv(rv$data$care_requests, "data/care_requests.csv")
    write_csv(rv$data$elderly_requests, "data/elderly_requests.csv")
    user_chat_history$elderly <- append(
      user_chat_history$elderly,
      list(
        div(
          style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
          strong("🤖 CareMatch Bot:"), " Request submitted successfully (ID: ", new_id, "). You can now use (R) to recommend a volunteer. Note: You may need to run the ML model (ml_training.R) to generate new match predictions.",
          tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
        )
      )
    )
    output$elderly_chatbot_flow <- renderUI({
      tagList(
        user_chat_history$elderly,
        actionButton("start_elderly_chatbot", "Restart Chat")
      )
    })
  })
  
  # ---- Volunteer Chatbot Flow ----
  output$volunteer_chatbot_ui <- renderUI({
    if (is.null(user$type) || user$type != "volunteer" || is.null(user$id)) {
      tagList(
        p("Please log in with a valid Volunteer ID to use the chatbot.")
      )
    } else {
      tagList(
        actionButton("start_volunteer_chatbot", "Start Chatbot 💬", class = "btn-warning"),
        uiOutput("volunteer_chatbot_flow")
      )
    }
  })
  
  observeEvent(input$start_volunteer_chatbot, {
    user$step <- "start"
    user_chat_history$volunteer <- list(
      div(
        style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
        strong("🤖 CareMatch Bot:"), " Welcome ", user$data$name %||% "User", "! What would you like to do?",
        tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
      )
    )
    output$volunteer_chatbot_flow <- renderUI({
      tagList(
        user_chat_history$volunteer,
        radioButtons(
          "volunteer_choice",
          "Options:",
          choices = c(
            "(V) View matches" = "V",
            "(S) Set availability" = "S",
            "(E) Extend radius" = "E"
          )
        ),
        actionButton("volunteer_next", "Send")
      )
    })
  })
  
  observeEvent(input$volunteer_next, {
    req(input$volunteer_choice)
    choice <- input$volunteer_choice
    user_chat_history$volunteer <- append(
      user_chat_history$volunteer,
      list(
        div(
          style = "background-color:#dcf8c6; padding:10px; border-radius:10px; margin-left:40px;",
          strong("🤝 You:"), paste(choice),
          tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
        )
      )
    )
    if (choice == "V") {
      tryCatch({
        # Validate user$id
        if (is.null(user$id) || user$id == "") {
          stop("User ID is not set. Please log in again.")
        }
        
        # Trim user$id and validate carematch_predictions
        user_id_trimmed <- str_trim(user$id)
        if (nrow(rv$data$carematch_predictions) == 0) {
          stop("No predictions available. Please run ml_training.R to generate carematch_predictions.csv.")
        }
        
        # Filter matches
        matches <- rv$data$carematch_predictions %>%
          filter(str_trim(volunteer_id) == user_id_trimmed) %>%
          select(request_id, match_prob, distance_km, skill_match, response_rate, time_match) %>%
          arrange(desc(match_prob))
        
        if (nrow(matches) == 0) {
          user_chat_history$volunteer <- append(
            user_chat_history$volunteer,
            list(
              div(
                style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
                strong("🤖 CareMatch Bot:"), " No matches found for volunteer ID ", user_id_trimmed, ". This might be because no care requests match your skills, availability, or location. Try extending your radius (E) or updating your availability (S). Also, ensure ml_training.R has been run recently to update predictions.",
                tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
              )
            )
          )
          output$volunteer_chatbot_flow <- renderUI({
            tagList(
              user_chat_history$volunteer,
              actionButton("start_volunteer_chatbot", "Restart Chat")
            )
          })
        } else {
          output$volunteer_chatbot_flow <- renderUI({
            tagList(
              user_chat_history$volunteer,
              div(
                style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
                strong("🤖 CareMatch Bot:"), " Here are your matches:",
                tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
              ),
              DTOutput("volunteer_matches_table"),
              actionButton("start_volunteer_chatbot", "Restart Chat")
            )
          })
          output$volunteer_matches_table <- renderDT({
            datatable(
              matches,
              options = list(
                pageLength = 5,
                order = list(list(1, "desc"))
              ),
              colnames = c(
                "Request ID", "Match Probability", "Distance (km)",
                "Skill Match", "Response Rate", "Time Match"
              )
            ) %>%
              formatPercentage(
                c("match_prob", "skill_match", "response_rate", "time_match"),
                digits = 2
              ) %>%
              formatRound("distance_km", digits = 2) %>%
              formatStyle(
                "time_match",
                backgroundColor = styleInterval(0.8, c("white", "lightgreen"))
              )
          })
        }
      }, error = function(e) {
        user_chat_history$volunteer <- append(
          user_chat_history$volunteer,
          list(
            div(
              style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
              strong("🤖 CareMatch Bot:"), " An error occurred while fetching matches: ", e$message, ". Please ensure ml_training.R has been run and your volunteer ID is valid.",
              tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
            )
          )
        )
        output$volunteer_chatbot_flow <- renderUI({
          tagList(
            user_chat_history$volunteer,
            actionButton("start_volunteer_chatbot", "Restart Chat")
          )
        })
      })
    } else if (choice == "S") {
      current_slots <- rv$data$availability %>%
        filter(volunteer_id == user$id) %>%
        pull(time_slot)
      output$volunteer_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$volunteer,
          checkboxGroupInput(
            "new_slots",
            "Select Time Slots:",
            choices = rv$data$time_slots,
            selected = current_slots
          ),
          actionButton("save_slots", "Save Availability")
        )
      })
    } else if (choice == "E") {
      output$volunteer_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$volunteer,
          sliderInput(
            "new_radius",
            "Service Radius (km):",
            min = 5,
            max = 30,
            value = user$data$radius_km %||% user$data$radius_willingness
          ),
          actionButton("save_radius", "Update Radius")
        )
      })
    }
  })
  
  observeEvent(input$save_slots, {
    rv$data$availability <- rv$data$availability %>%
      filter(volunteer_id != user$id)
    if (length(input$new_slots) > 0) {
      new_rows <- tibble(
        volunteer_id = user$id,
        time_slot = input$new_slots,
        reliability = 1.0 # Default reliability
      )
      rv$data$availability <- bind_rows(rv$data$availability, new_rows)
    }
    write_csv(rv$data$availability, "data/availability.csv")
    user_chat_history$volunteer <- append(
      user_chat_history$volunteer,
      list(
        div(
          style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
          strong("🤖 CareMatch Bot:"), " Availability updated.",
          tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
        )
      )
    )
    output$volunteer_chatbot_flow <- renderUI({
      tagList(
        user_chat_history$volunteer,
        actionButton("start_volunteer_chatbot", "Restart Chat")
      )
    })
  })
  
  observeEvent(input$save_radius, {
    rv$data$volunteers <- rv$data$volunteers %>%
      mutate(
        radius_km = if_else(volunteer_id == user$id, as.numeric(input$new_radius), radius_km),
        radius_willingness = if_else(volunteer_id == user$id, as.numeric(input$new_radius), radius_willingness)
      )
    write_csv(rv$data$volunteers, "data/volunteers.csv")
    user$data$radius_km <- input$new_radius
    user$data$radius_willingness <- input$new_radius
    user_chat_history$volunteer <- append(
      user_chat_history$volunteer,
      list(
        div(
          style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
          strong("🤖 CareMatch Bot:"), paste(" Radius updated to ", input$new_radius, " km"),
          tags$span(style = "float:right; font-size:0.8em;", get_timestamp())
        )
      )
    )
    output$volunteer_chatbot_flow <- renderUI({
      tagList(
        user_chat_history$volunteer,
        actionButton("start_volunteer_chatbot", "Restart Chat")
      )
    })
  })
  
  # ---- Manage Elderly Profiles ----
  output$manage_elderly_ui <- renderUI({
    if (input$manage_elderly_id == "new") {
      tagList(
        box(
          width = 12,
          title = "Create New Elderly Profile",
          fluidRow(
            column(6,
                   textInput("elderly_name", "Full Name:", placeholder = "e.g., John Doe"),
                   bsTooltip("elderly_name", "Enter the full name of the elderly person.", placement = "right")
            ),
            column(6,
                   numericInput("elderly_age", "Age:", min = 65, max = 120, value = 65),
                   bsTooltip("elderly_age", "Enter age (65 or older).", placement = "right")
            )
          ),
          fluidRow(
            column(6,
                   selectizeInput(
                     "elderly_zip",
                     "Zipcode:",
                     choices = rv$data$unique_zipcodes,
                     options = list(placeholder = "e.g., 12345")
                   ),
                   bsTooltip("elderly_zip", "Select a valid zipcode.", placement = "right")
            ),
            column(6,
                   selectizeInput(
                     "elderly_time_slots",
                     "Preferred Time Slots:",
                     choices = rv$data$time_slots,
                     multiple = TRUE,
                     options = list(placeholder = "e.g., Weekday AM")
                   ),
                   bsTooltip("elderly_time_slots", "Select preferred time slots.", placement = "right")
            )
          ),
          textInput("elderly_contact", "Contact Number:", placeholder = "e.g., 123-456-7890"),
          selectizeInput(
            "elderly_needs",
            "Select Needs:",
            choices = setNames(rv$data$needs$need_id, rv$data$needs$need_name),
            multiple = TRUE
          ),
          actionButton("create_elderly", "Create Profile", class = "btn-primary")
        )
      )
    } else {
      elderly_id <- as.character(input$manage_elderly_id)
      rv$elderly_profile <- rv$data$elderly %>% filter(elderly_id == !!elderly_id)
      tagList(
        tabBox(
          width = 12,
          tabPanel(
            "Dashboard",
            h4("Elderly Profile Dashboard")
          ),
          tabPanel(
            "Elderly Profile",
            DTOutput("elderly_profile_table"),
            br(),
            
            
            h4("Manage Needs"),
            DTOutput("elderly_needs_table"),
            selectizeInput(
              "add_elderly_needs",
              "Add Needs:",
              choices = setNames(rv$data$needs$need_id, rv$data$needs$need_name),
              multiple = TRUE
            ),
            actionButton("update_elderly_needs", "Update Needs", class = "btn-primary")
          )
        )
      )
    }
  })
  
  observeEvent(input$create_elderly, {
    req(input$elderly_name, input$elderly_age, input$elderly_zip, input$elderly_time_slots)
    tryCatch({
      if (str_trim(input$elderly_name) == "") {
        showNotification("Name cannot be empty.", type = "error")
        return()
      }
      # Look up zipcode data from the dataset
      zip_data <- rv$data$zipcode_data %>%
        filter(zipcode == input$elderly_zip)
      
      if (nrow(zip_data) == 0) {
        showNotification("Selected zipcode not found in dataset.", type = "error")
        return()
      }
      
      # Extract numeric part of existing elderly_ids, remove the "E" prefix
      existing_ids <- as.numeric(gsub("E", "", rv$data$elderly$elderly_id))
      new_id <- max(existing_ids, 0, na.rm = TRUE) + 1
      new_elderly_id <- paste0("E", new_id)  # Add "E" prefix
      
      new_elderly <- tibble(
        elderly_id = new_elderly_id,
        name = input$elderly_name,
        age = as.integer(input$elderly_age),
        zipcode = input$elderly_zip,
        location = input$elderly_zip,
        contact_number = input$elderly_contact %||% "Not provided",
        preferred_time_slots = paste(input$elderly_time_slots, collapse = ", "),
        major_city = zip_data$major_city[1],
        state = zip_data$state[1],
        lat = zip_data$lat[1],
        lng = zip_data$lng[1]
      )
      rv$data$elderly <- bind_rows(rv$data$elderly, new_elderly)
      if (length(input$elderly_needs) > 0) {
        new_needs <- tibble(
          elderly_id = rep(new_elderly_id, length(input$elderly_needs)),
          need_id = as.numeric(input$elderly_needs)
        )
        rv$data$elderly_needs <- bind_rows(rv$data$elderly_needs, new_needs)
      }
      write_csv(rv$data$elderly, "data/elderly.csv")
      write_csv(rv$data$elderly_needs, "data/elderly_needs.csv")
      updateSelectInput(
        session,
        "manage_elderly_id",
        choices = c("New user..." = "new", setNames(rv$data$elderly$elderly_id,
                                                    paste(rv$data$elderly$name, " (ID:", data$elderly$elderly_id, ")"))),
        selected = new_elderly_id
      )
      showNotification("Profile created successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error creating elderly profile:", e$message), type = "error")
    })
  })
  
  output$elderly_profile_table <- renderDT({
    req(rv$elderly_profile)
    rv$elderly_profile %>%
      select(name, age, major_city, state, preferred_time_slots, contact_number) %>%
      rename(
        "Name" = name,
        "Age" = age,
        "City" = major_city,
        "State" = state,
        "Preferred Time Slots" = preferred_time_slots,
        "Contact" = contact_number
      ) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  output$elderly_needs_table <- renderDT({
    req(rv$elderly_profile)
    rv$data$elderly_needs %>%
      filter(elderly_id == rv$elderly_profile$elderly_id) %>%
      left_join(rv$data$needs, by = "need_id") %>%
      select(need_id, need_name) %>%
      rename(
        "Need ID" = need_id,
        "Need Name" = need_name
      ) %>%
      datatable(options = list(pageLength = 5), rownames = FALSE)
  })
  
  observeEvent(input$update_elderly_needs, {
    req(rv$elderly_profile, input$add_elderly_needs)
    rv$data$elderly_needs <- rv$data$elderly_needs %>%
      filter(elderly_id != rv$elderly_profile$elderly_id)
    if (length(input$add_elderly_needs) > 0) {
      new_needs <- tibble(
        elderly_id = rep(rv$elderly_profile$elderly_id, length(input$add_elderly_needs)),
        need_id = as.numeric(input$add_elderly_needs)
      )
      rv$data$elderly_needs <- bind_rows(rv$data$elderly_needs, new_needs)
    }
    write_csv(rv$data$elderly_needs, "data/elderly_needs.csv")
    showNotification("Needs updated successfully!", type = "message")
  })
  
  # ---- Manage Volunteer Profiles ----
  output$manage_volunteer_ui <- renderUI({
    if (input$manage_volunteer_id == "new") {
      tagList(
        box(
          width = 12,
          title = "Create New Volunteer Profile",
          fluidRow(
            column(6,
                   textInput("volunteer_name", "Full Name:", placeholder = "e.g., Jane Smith"),
                   bsTooltip("volunteer_name", "Enter the full name of the volunteer.", placement = "right")
            ),
            column(6,
                   selectizeInput(
                     "volunteer_zip",
                     "Zipcode:",
                     choices = rv$data$unique_zipcodes,
                     options = list(placeholder = "e.g., 12345")
                   ),
                   bsTooltip("volunteer_zip", "Select a valid zipcode.", placement = "right")
            )
          ),
          fluidRow(
            column(6,
                   selectizeInput(
                     "volunteer_availability",
                     "Availability:",
                     choices = rv$data$time_slots,
                     multiple = TRUE,
                     options = list(placeholder = "e.g., Weekday AM")
                   ),
                   bsTooltip("volunteer_availability", "Select available time slots.", placement = "right")
            ),
            column(6,
                   numericInput("volunteer_radius", "Maximum distance (km):", min = 1, max = 100, value = 10),
                   bsTooltip("volunteer_radius", "Enter maximum travel distance in km.", placement = "right")
            )
          ),
          textInput("volunteer_contact", "Contact Number:", placeholder = "e.g., 123-456-7890"),
          selectizeInput(
            "volunteer_skills",
            "Your Skills:",
            choices = setNames(rv$data$skills$skill_id, rv$data$skills$skill_name),
            multiple = TRUE
          ),
          actionButton("create_volunteer", "Create Profile", class = "btn-success")
        )
      )
    } else {
      volunteer_id <- as.character(input$manage_volunteer_id)
      rv$volunteer_profile <- rv$data$volunteers %>% filter(volunteer_id == !!volunteer_id)
      tagList(
        tabBox(
          width = 12,
          tabPanel(
            "Dashboard",
            h4("Volunteer Profile Dashboard")
          ),
          tabPanel(
            "Volunteer Profile",
            DTOutput("volunteer_profile_table"),
            br(),
            h4("Manage Skills"),
            DTOutput("volunteer_skills_table"),
            selectizeInput(
              "add_volunteer_skills",
              "Add Skills:",
              choices = setNames(rv$data$skills$skill_id, rv$data$skills$skill_name),
              multiple = TRUE
            ),
            actionButton("update_volunteer_skills", "Update Skills", class = "btn-primary")
          ),
          tabPanel(
            "View Matches",
            selectInput(
              "view_matches_request_id",
              "Select Care Request ID:",
              choices = sort(unique(rv$data$care_requests$id)),
              selected = NULL
            ),
            DTOutput("volunteer_view_matches_table")
          ),
          tabPanel(
            "Recommend Volunteer",
            selectInput(
              "recommend_request_id",
              "Select Care Request ID:",
              choices = sort(unique(rv$data$care_requests$id)),
              selected = NULL
            ),
            actionButton("recommend_volunteer_btn", "Recommend Volunteer", class = "btn-primary"),
            verbatimTextOutput("volunteer_recommendation")
          )
        )
      )
    }
  })
  
  observeEvent(input$create_volunteer, {
    req(input$volunteer_name, input$volunteer_zip, input$volunteer_availability, input$volunteer_radius)
    tryCatch({
      if (str_trim(input$volunteer_name) == "") {
        showNotification("Name cannot be empty.", type = "error")
        return()
      }
      # Look up zipcode data from the dataset
      zip_data <- rv$data$zipcode_data %>%
        filter(zipcode == input$volunteer_zip)
      
      if (nrow(zip_data) == 0) {
        showNotification("Selected zipcode not found in dataset.", type = "error")
        return()
      }
      
      # Extract numeric part of existing volunteer_ids, remove the "V" prefix
      existing_ids <- as.numeric(gsub("V", "", rv$data$volunteers$volunteer_id))
      new_id <- max(existing_ids, 0, na.rm = TRUE) + 1
      new_volunteer_id <- paste0("V", new_id)  # Add "V" prefix
      
      new_volunteer <- tibble(
        volunteer_id = new_volunteer_id,
        name = input$volunteer_name,
        zipcode = input$volunteer_zip,
        location = input$volunteer_zip,
        contact_number = input$volunteer_contact %||% "Not provided",
        availability = paste(input$volunteer_availability, collapse = ", "),
        radius_km = as.double(input$volunteer_radius),
        radius_willingness = as.double(input$volunteer_radius),
        major_city = zip_data$major_city[1],
        state = zip_data$state[1],
        lat = zip_data$lat[1],
        lng = zip_data$lng[1],
        response_rate = 0.0,
        skills = paste(input$volunteer_skills, collapse = ",") %||% "None"
      )
      rv$data$volunteers <- bind_rows(rv$data$volunteers, new_volunteer)
      if (length(input$volunteer_skills) > 0) {
        new_skills <- tibble(
          volunteer_id = rep(new_volunteer_id, length(input$volunteer_skills)),
          skill_id = as.numeric(input$volunteer_skills)
        )
        rv$data$volunteer_skills <- bind_rows(rv$data$volunteer_skills, new_skills)
      }
      write_csv(rv$data$volunteers, "data/volunteers.csv")
      write_csv(rv$data$volunteer_skills, "data/volunteer_skills.csv")
      updateSelectInput(
        session,
        "manage_volunteer_id",
        choices = c("New volunteer..." = "new", setNames(rv$data$volunteers$volunteer_id,
                                                         paste(rv$data$volunteers$name, " (ID:", rv$data$volunteers$volunteer_id, ")"))),
        selected = new_volunteer_id
      )
      showNotification("Profile created successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error creating volunteer profile:", e$message), type = "error")
    })
  })
  
  output$volunteer_profile_table <- renderDT({
    req(rv$volunteer_profile)
    rv$volunteer_profile %>%
      select(name, major_city, state, availability, radius_km, contact_number) %>%
      rename(
        "Name" = name,
        "City" = major_city,
        "State" = state,
        "Availability" = availability,
        "Max Distance (km)" = radius_km,
        "Contact" = contact_number
      ) %>%
      datatable(options = list(dom = 't'), rownames = FALSE)
  })
  
  output$volunteer_skills_table <- renderDT({
    req(rv$volunteer_profile)
    rv$data$volunteer_skills %>%
      filter(volunteer_id == rv$volunteer_profile$volunteer_id) %>%
      left_join(rv$data$skills, by = "skill_id") %>%
      select(skill_id, skill_name) %>%
      rename(
        "Skill ID" = skill_id,
        "Skill Name" = skill_name
      ) %>%
      datatable(options = list(pageLength = 5), rownames = FALSE)
  })
  
  observeEvent(input$update_volunteer_skills, {
    req(rv$volunteer_profile, input$add_volunteer_skills)
    rv$data$volunteer_skills <- rv$data$volunteer_skills %>%
      filter(volunteer_id != rv$volunteer_profile$volunteer_id)
    if (length(input$add_volunteer_skills) > 0) {
      new_skills <- tibble(
        volunteer_id = rep(rv$volunteer_profile$volunteer_id, length(input$add_volunteer_skills)),
        skill_id = as.numeric(input$add_volunteer_skills)
      )
      rv$data$volunteer_skills <- bind_rows(rv$data$volunteer_skills, new_skills)
    }
    write_csv(rv$data$volunteer_skills, "data/volunteer_skills.csv")
    showNotification("Skills updated successfully!", type = "message")
  })
  
  # ---- Volunteer Profile: View Matches ----
  filtered_volunteer_predictions <- reactive({
    req(input$view_matches_request_id)
    rv$data$carematch_predictions %>%
      filter(request_id == input$view_matches_request_id) %>%
      arrange(desc(match_prob))
  })
  
  output$volunteer_view_matches_table <- renderDT({
    req(input$view_matches_request_id)
    datatable(
      filtered_volunteer_predictions(),
      options = list(
        pageLength = 10,
        order = list(list(3, "desc")) # Sort by match_prob
      ),
      colnames = c(
        "Volunteer ID", "Request ID", "Elderly ID", "Match Probability",
        "Distance (km)", "Skill Match", "Response Rate", "Time Match", "Urgency"
      )
    ) %>%
      formatPercentage(
        c("match_prob", "skill_match", "response_rate", "time_match"),
        digits = 2
      ) %>%
      formatRound("distance_km", digits = 2) %>%
      formatStyle(
        "time_match",
        backgroundColor = styleInterval(0.8, c("white", "lightgreen"))
      )
  })
  
  # ---- Volunteer Profile: Recommend Volunteer ----
  filtered_recommend_predictions <- reactive({
    req(input$recommend_request_id)
    rv$data$carematch_predictions %>%
      filter(request_id == input$recommend_request_id) %>%
      arrange(desc(match_prob))
  })
  
  output$volunteer_recommendation <- renderText({
    req(input$recommend_request_id, input$recommend_volunteer_btn)
    isolate({
      top_match <- filtered_recommend_predictions() %>%
        slice_head(n = 1)
      
      if (nrow(top_match) == 0) {
        return("No matches found for this request.")
      }
      
      paste0(
        "Recommended Volunteer for Request ", top_match$request_id, ":\n",
        "Volunteer ID: ", top_match$volunteer_id, "\n",
        "Match Probability: ", sprintf("%.2f%%", top_match$match_prob * 100), "\n",
        "Distance: ", sprintf("%.2f km", top_match$distance_km), "\n",
        "Skill Match: ", sprintf("%.2f%%", top_match$skill_match * 100), "\n",
        "Response Rate: ", sprintf("%.2f%%", top_match$response_rate * 100), "\n",
        "Time Match: ", sprintf("%.2f%%", top_match$time_match * 100), "\n",
        "Urgency: ", top_match$urgency
      )
    })
  })
}

# ---- Run the App ----
shinyApp(ui, server)
