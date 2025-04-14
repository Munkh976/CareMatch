library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(geosphere)
library(readr)
library(lubridate)
library(caret)

# ---- Load Data ----
data <- list(
  elderly = read_csv("data/elderly.csv"),
  volunteers = read_csv("data/volunteers.csv"),
  care_requests = read_csv("data/care_requests.csv"),
  elderly_requests = read_csv("data/elderly_requests.csv"),
  volunteer_responses = read_csv("data/volunteer_responses.csv"),
  historical_matches = read_csv("data/historical_matches.csv"),
  availability = read_csv("data/availability.csv")
)

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
      menuItem("Volunteer Portal", tabName = "volunteer", icon = icon("hands-helping"))
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
            tabPanel("My Requests", DTOutput("elderly_requests")),
            tabPanel("History", DTOutput("elderly_history")),
            tabPanel(
              "Chatbot",
              actionButton("start_elderly_chatbot", "Start Chatbot 💬", class = "btn-warning"),
              uiOutput("elderly_chatbot_flow")
            )
          )
        )
      ),
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
              actionButton("start_volunteer_chatbot", "Start Chatbot 💬", class = "btn-warning"),
              uiOutput("volunteer_chatbot_flow")
            )
          )
        )
      )
    )
  )
)

# -------------------------------
# Server Logic
# -------------------------------
server <- function(input, output, session) {
  user <- reactiveValues(
    type = NULL,
    id = NULL,
    data = NULL,
    step = NULL,
    active_requests = NULL,
    match_targets = NULL,
    top_requests = NULL
  )
  
  # ---- Login navigation ----
  observeEvent(input$login_elderly, {
    updateTabItems(session, "tabs", "elderly")
  })
  
  observeEvent(input$login_volunteer, {
    updateTabItems(session, "tabs", "volunteer")
  })
  
  # ---- Elderly Login ----
  observeEvent(input$elderly_login, {
    if (input$elderly_id %in% data$elderly$id) {
      user$type <- "elderly"
      user$id <- input$elderly_id
      user$data <- data$elderly %>% filter(id == input$elderly_id)
      showNotification("Elderly login successful", type = "message")
    } else {
      showNotification("Invalid ID", type = "error")
    }
  })
  
  # ---- Volunteer Login ----
  observeEvent(input$volunteer_login, {
    if (input$volunteer_id %in% data$volunteers$id) {
      user$type <- "volunteer"
      user$id <- input$volunteer_id
      user$data <- data$volunteers %>% filter(id == input$volunteer_id)
      showNotification("Volunteer login successful", type = "message")
    } else {
      showNotification("Invalid ID", type = "error")
    }
  })
  
  # ---- Elderly Portal Components ----
  output$elderly_profile <- renderUI({
    req(user$type == "elderly")
    u <- user$data
    tagList(
      h3(u$name),
      p(strong("Location:"), u$location),
      p(strong("Contact:"), u$contact_number)
    )
  })
  
  output$elderly_requests <- DT::renderDataTable({
    req(user$type == "elderly")
    
    pending <- data$care_requests %>%
      filter(elderly_id == user$id) %>%
      left_join(data$elderly_requests, by = c("id" = "request_id")) %>%
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
      rownames = FALSE, 
      options = list(dom = 't')
    )
  })
  
  output$elderly_history <- renderDT({
    req(user$type == "elderly")
    data$elderly_requests %>%
      filter(elderly_id == user$id, status == "completed") %>%
      left_join(data$care_requests, by = c("request_id" = "id")) %>%
      select(request_id, needs, time_prefs, urgency, modified_at)
  })
  
  # ---- Volunteer Portal Components ----
  output$volunteer_profile <- renderUI({
    req(user$type == "volunteer")
    u <- user$data
    tagList(
      h3(u$name),
      p(strong("Location:"), u$location),
      p(strong("Contact:"), u$contact_number),
      p(strong("Skills:"), u$skills),
      p(strong("Response Rate:"), paste0(u$response_rate * 100, "%")),
      p(strong("Radius:"), paste(u$radius_km, "km"))
    )
  })
  
  output$volunteer_inbox <- renderDT({
    req(user$type == "volunteer")
    data$volunteer_responses %>%
      filter(volunteer_id == user$id) %>%
      left_join(data$care_requests, by = c("request_id" = "id")) %>%
      left_join(data$elderly, by = c("elderly_id" = "id")) %>%
      select(request_id, elderly_name = name, needs, time_prefs, urgency, status)
  })
  
  output$volunteer_sent <- renderDT({
    req(user$type == "volunteer")
    data$volunteer_responses %>%
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
    responses <- data$volunteer_responses %>%
      filter(request_id == req_id, status == "accepted") %>%
      left_join(data$volunteers, by = c("volunteer_id" = "id"))
    
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
    data$elderly_requests <- data$elderly_requests %>%
      filter(request_id != req_id)
    showNotification("Request cancelled", type = "message")
  })
  
  # Dynamic accept buttons
  observe({
    lapply(data$volunteer_responses$volunteer_id, function(vol_id) {
      observeEvent(input[[paste0("accept_vol_", vol_id)]], {
        showNotification(paste("You accepted", vol_id, "! Match confirmed."), type = "message")
        removeModal()
      })
    })
  })
    
    # ---------------- Volunteer Chatbot Flow ----------------
    observeEvent(input$start_volunteer_chatbot, {
      user$step <- "start"
      user_chat_history$volunteer <- list(
        div(
          style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
          strong("🤖 CareMatch Bot:"), " Welcome ", user$data$name, "! What would you like to do?",
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
        top_matches <- data$historical_matches %>%
          filter(volunteer_id == user$id & predicted_probability >= 0.7) %>%
          arrange(desc(predicted_probability)) %>% 
          head(3)
        
        user$top_requests <- top_matches
        
        match_msgs <- lapply(1:nrow(top_matches), function(i) {
          req <- top_matches[i, ]
          paste0(
            i, ". Request ", req$request_id, " – ", req$needs,
            " (", round(req$predicted_probability * 100), "%)"
          )
        })
        
        output$volunteer_chatbot_flow <- renderUI({
          tagList(
            user_chat_history$volunteer,
            p("Top Matches:"),
            HTML(paste(match_msgs, collapse = "<br>")),
            textInput("accept_match_index", "Accept which match? (1/2/3/all)", ""),
            actionButton("accept_match_btn", "Accept")
          )
        })
      }
      
      if (choice == "S") {
        current_slots <- data$availability %>% 
          filter(volunteer_id == user$id) %>% 
          pull(time_slot)
        
        output$volunteer_chatbot_flow <- renderUI({
          tagList(
            user_chat_history$volunteer,
            checkboxGroupInput(
              "new_slots",
              "Select Time Slots:",
              choices = unique(data$availability$time_slot),
              selected = current_slots
            ),
            actionButton("save_slots", "Save Availability")
          )
        })
      }
      
      if (choice == "E") {
        output$volunteer_chatbot_flow <- renderUI({
          tagList(
            user_chat_history$volunteer,
            sliderInput(
              "new_radius",
              "Service Radius (km):",
              min = 5,
              max = 30,
              value = user$data$radius_km
            ),
            actionButton("save_radius", "Update Radius")
          )
        })
      }
    })
    
    observeEvent(input$accept_match_btn, {
      selected_idx <- tolower(trimws(input$accept_match_index))
      if (selected_idx == "all") {
        selected <- user$top_requests
      } else {
        selected <- user$top_requests[as.integer(selected_idx), ]
      }
      
      for (i in 1:nrow(selected)) {
        data$volunteer_responses <- bind_rows(
          data$volunteer_responses,
          tibble(
            id = paste0("VR_", nrow(data$volunteer_responses) + 1),
            request_id = selected$request_id[i],
            volunteer_id = user$id,
            status = "accepted",
            responded_at = Sys.time()
          )
        )
      }
      
      output$volunteer_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$volunteer,
          p("✅ Request(s) accepted successfully!"),
          actionButton("start_volunteer_chatbot", "Restart Chat")
        )
      })
    })
    
    observeEvent(input$save_slots, {
      data$availability <- data$availability %>% 
        filter(volunteer_id != user$id)
      
      new_rows <- tibble(
        volunteer_id = user$id,
        time_slot = input$new_slots
      )
      
      data$availability <- bind_rows(data$availability, new_rows)
      
      output$volunteer_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$volunteer,
          p("✅ Availability updated."),
          actionButton("start_volunteer_chatbot", "Restart Chat")
        )
      })
    })
    
    observeEvent(input$save_radius, {
      data$volunteers$radius_km[data$volunteers$id == user$id] <- input$new_radius
      output$volunteer_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$volunteer,
          p(paste("✅ Radius updated to", input$new_radius, "km")),
          actionButton("start_volunteer_chatbot", "Restart Chat")
        )
      })
    })
    
    # ---------------- Elderly Chatbot Flow ----------------
    observeEvent(input$start_elderly_chatbot, {
      user$step <- "start"
      user_chat_history$elderly <- list(
        div(
          style = "background-color:#f1f1f1; padding:10px; border-radius:10px;",
          strong("🤖 CareMatch Bot:"), " Welcome ", user$data$name, "! How can I help?",
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
              "(R) Recommend volunteer" = "R",
              "(A) Add request" = "A"
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
            textInput("need_type", "Need Type:"),
            textInput("time_slot", "Time Slot (e.g., Mon 9-12):"),
            selectInput(
              "urgency",
              "Urgency:",
              choices = c("Low", "Medium", "High")
            ),
            actionButton("submit_request", "Submit Request")
          )
        })
      }
      
      if (choice == "R") {
        active_requests <- data$elderly_requests %>%
          filter(elderly_id == user$id, status == "pending") %>%
          left_join(data$care_requests, by = c("request_id" = "id"))
        
        user$active_requests <- active_requests
        
        output$elderly_chatbot_flow <- renderUI({
          tagList(
            user_chat_history$elderly,
            p("Your Active Requests:"),
            HTML(
              paste0(
                "<ol>",
                paste0(
                  sprintf(
                    "<li>%s (%s)</li>",
                    active_requests$request_id,
                    active_requests$needs
                  ),
                  collapse = ""
                ),
                "</ol>"
              )
            ),
            textInput("recommend_index", "Select request index to match (1/2/...):"),
            actionButton("recommend_match_btn", "Recommend")
          )
        })
      }
    })
    
    observeEvent(input$submit_request, {
      new_id <- paste0("REQ_", nrow(data$care_requests) + 1)
      data$care_requests <- bind_rows(
        data$care_requests,
        tibble(
          id = new_id,
          elderly_id = user$id,
          needs = input$need_type,
          time_prefs = input$time_slot,
          urgency = input$urgency
        )
      )
      
      data$elderly_requests <- bind_rows(
        data$elderly_requests,
        tibble(
          request_id = new_id,
          elderly_id = user$id,
          status = "pending",
          requested_volunteers = "",
          modified_at = Sys.time()
        )
      )
      
      output$elderly_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$elderly,
          p("✅ Request submitted."),
          actionButton("start_elderly_chatbot", "Restart Chat")
        )
      })
    })
    
    observeEvent(input$recommend_match_btn, {
      idx <- as.integer(input$recommend_index)
      req(idx <= nrow(user$active_requests))
      req_row <- user$active_requests[idx, ]
      
      top_matches <- data$historical_matches %>%
        filter(request_id == req_row$request_id) %>%
        arrange(desc(predicted_probability)) %>% 
        head(3)
      
      user$match_targets <- top_matches
      match_list <- lapply(1:nrow(top_matches), function(i) {
        m <- top_matches[i, ]
        paste0(
          i, ". ", m$volunteer_id, " – ", m$needs, " – ",
          round(m$predicted_probability * 100), "%"
        )
      })
      
      output$elderly_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$elderly,
          p("Top Volunteers:"),
          HTML(paste(match_list, collapse = "<br>")),
          textInput("send_match_index", "Send request to (1/2/3/all):", ""),
          actionButton("send_match_btn", "Send Request")
        )
      })
    })
    
    observeEvent(input$send_match_btn, {
      sel <- tolower(trimws(input$send_match_index))
      if (sel == "all") {
        selected <- user$match_targets$volunteer_id
      } else {
        selected <- user$match_targets$volunteer_id[as.integer(sel)]
      }
      
      data$elderly_requests$requested_volunteers[
        data$elderly_requests$request_id == 
          user$active_requests$request_id[as.integer(input$recommend_index)]
      ] <- paste(selected, collapse = ",")
      
      output$elderly_chatbot_flow <- renderUI({
        tagList(
          user_chat_history$elderly,
          p(paste("✅ Request sent to:", paste(selected, collapse = ", "))),
          actionButton("start_elderly_chatbot", "Restart Chat")
        )
      })
    })
}

shinyApp(ui, server)
