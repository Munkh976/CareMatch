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

# ---- UI Definition ----
ui <- dashboardPage(
  dashboardHeader(title = "CareMatch"),
  dashboardSidebar(
    id = "tabs",
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Elderly Portal", tabName = "elderly", icon = icon("user")),
      menuItem("Volunteer Portal", tabName = "volunteer", icon = icon("hands-helping"))
    )
  ),
  dashboardBody(
    # JS Handler
    tags$head(tags$script(HTML("
      Shiny.addCustomMessageHandler('acceptHandler', function(x) {
        $(document).on('click', 'button[id^=accept_]', function() {
          var id = $(this).attr('id').replace('accept_', '');
          Shiny.setInputValue('accept_request_id', id, {priority: 'event'});
        });
      });
    "))),
    tabItems(
      tabItem(
        tabName = "home",
        fluidRow(
          box(width = 12,
              h1("Welcome to CareMatch!"),
              p("Connecting elderly individuals with volunteers for caregiving."),
              actionButton("login_elderly", "Elderly Login", class = "btn-primary"),
              actionButton("login_volunteer", "Volunteer Login", class = "btn-success"))
        )
      ),
      tabItem(
        tabName = "elderly",
        fluidRow(box(width = 12,
                     textInput("elderly_id", "Enter Elderly ID"),
                     actionButton("elderly_login", "Login"))),
        fluidRow(
          tabBox(width = 12,
                 tabPanel("Profile", uiOutput("elderly_profile")),
                 tabPanel("My Requests", DTOutput("elderly_requests")),
                 tabPanel("History", DTOutput("elderly_history")),
                 tabPanel("Chatbot",
                          actionButton("start_elderly_chatbot", "Start Chatbot 💬", class = "btn-warning"),
                          uiOutput("elderly_chatbot_flow"))
          )
        )
      ),
      tabItem(
        tabName = "volunteer",
        fluidRow(box(width = 12,
                     textInput("volunteer_id", "Enter Volunteer ID"),
                     actionButton("volunteer_login", "Login"))),
        fluidRow(
          tabBox(width = 12,
                 tabPanel("Profile", uiOutput("volunteer_profile")),
                 tabPanel("Inbox", DTOutput("volunteer_inbox")),
                 tabPanel("Sent Offers", DTOutput("volunteer_sent")),
                 tabPanel("Notifications", uiOutput("volunteer_notifications")),
                 tabPanel("Chatbot",
                          actionButton("start_volunteer_chatbot", "Start Chatbot 💬", class = "btn-warning"),
                          uiOutput("volunteer_chatbot_flow"))
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
  user <- reactiveValues(type = NULL, id = NULL, data = NULL)
  
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
  
  observeEvent(input$view_offers_request, {
    req_id <- input$view_offers_request
    responses <- data$volunteer_responses %>%
      filter(request_id == req_id, status == "accepted") %>%
      left_join(data$volunteers, by = c("volunteer_id" = "id"))
    
    if (nrow(responses) == 0) {
      showModal(modalDialog("No offers yet. Please check back later."))
      return()
    }
    
    showModal(modalDialog(
      title = paste("Offers for Request", req_id),
      size = "l",
      footer = modalButton("Close"),
      easyClose = TRUE,
      tagList(
        lapply(1:nrow(responses), function(i) {
          row <- responses[i, ]
          actionButton(paste0("accept_vol_", row$volunteer_id), 
                       paste("Accept Offer from", row$name),
                       class = "btn-success btn-sm")
        })
      )
    ))
  })
  
  observe({
    # Catch dynamic accept buttons
    lapply(data$volunteer_responses$volunteer_id, function(vol_id) {
      observeEvent(input[[paste0("accept_vol_", vol_id)]], {
        showNotification(paste("You accepted", vol_id, "! Match confirmed."), type = "message")
        removeModal()
      })
    })
  })
  
  
  observe({
    session$sendCustomMessage("offerHandler", list())
  })
  
  # JS code
  js_offer <- "
  Shiny.addCustomMessageHandler('offerHandler', function(x) {
    $(document).on('click', 'button[id^=offers_]', function() {
      var id = $(this).attr('id').replace('offers_', '');
      Shiny.setInputValue('view_offers_request', id, {priority: 'event'});
    });
    $(document).on('click', 'button[id^=cancel_]', function() {
      var id = $(this).attr('id').replace('cancel_', '');
      Shiny.setInputValue('cancel_request_id', id, {priority: 'event'});
    });
  });
  "
  tags$head(tags$script(HTML(js_offer)))
  
  
  # -------------------------------
  # Elderly Portal Components
  # -------------------------------
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
      mutate(Actions = paste0('<button id="offers_', id, '" class="btn btn-info btn-sm">Offers</button>',
                              ' <button id="cancel_', id, '" class="btn btn-danger btn-sm">Cancel</button>'))
    
    datatable(pending %>%
                select(id, needs, time_prefs, urgency, status, Actions),
              escape = FALSE, selection = 'none', rownames = FALSE, options = list(dom = 't'))
  })
  
  
  output$elderly_history <- renderDT({
    req(user$type == "elderly")
    data$elderly_requests %>%
      filter(elderly_id == user$id, status == "completed") %>%
      left_join(data$care_requests, by = c("request_id" = "id")) %>%
      select(request_id, needs, time_prefs, urgency, modified_at)
  })

  # -------------------------------
  # Volunteer Portal Components
  # -------------------------------
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
      mutate(Cancel = paste0('<button class="btn btn-danger btn-sm">Cancel</button>')) %>%
      datatable(escape = FALSE, selection = "none", options = list(dom = 'tp'))
  })
  
  output$volunteer_notifications <- renderDT({
    req(user$type == "volunteer")
    
    # Get open requests from elderly
    volunteer <- data$volunteers %>% filter(id == user$id)
    open_requests <- data$care_requests %>%
      left_join(data$elderly_requests, by = c("id" = "request_id")) %>%
      filter(status == "pending") %>%
      left_join(data$elderly, by = c("elderly_id" = "id"))
    
    available <- data$availability %>% filter(volunteer_id == user$id)
    
    matches <- open_requests %>%
      filter(!is.na(elderly_id)) %>%
      rowwise() %>%
      mutate(
        distance = calc_distance(lat, lng, volunteer$lat, volunteer$lng),
        skill_match = calc_skill_match(needs, volunteer$skills),
        time_overlap = any(strsplit(time_prefs, ", ")[[1]] %in% available$time_slot),
        eligible = distance <= volunteer$radius_km & time_overlap
      ) %>%
      filter(eligible) %>%
      ungroup() %>%
      select(request_id = id, needs, urgency, time_prefs, distance) %>%
      mutate(
        distance = round(distance, 2),
        Accept = paste0('<button class="btn btn-success btn-sm">Accept</button>')
      )
    
    datatable(matches, escape = FALSE, selection = "none", options = list(dom = 'tp'))
  })
  # -------------------------------
  # Chatbot Buttons
  # -------------------------------
  
  output$elderly_chatbot_ui <- renderUI({
    req(user$type == "elderly")
    HTML(paste0(
      "<p>🧓 Chatbot launched. How can I help you?</p><pre>",
      "1. (R) Recommend volunteers\n",
      "2. (A) Add new request",
      "</pre>"
    ))
  })
  
  output$volunteer_chatbot_ui <- renderUI({
    req(user$type == "volunteer")
    HTML(paste0(
      "<p>🤝 Chatbot launched. What would you like to do?</p><pre>",
      "R. Show ML Recommendations\n",
      "S. Set Availability\n",
      "E. Extend Radius",
      "</pre>"
    ))
  })
  
  
  # ----- Elderly: Offers Tab -----
  observeEvent(input$elderly_offers_btn, {
    req(user$type == "elderly")
    showModal(modalDialog(
      title = "Volunteer Offers for Your Requests",
      size = "l",
      easyClose = TRUE,
      uiOutput("elderly_offers_ui")
    ))
  })

  
  # ----- Volunteer: Notifications -----
  output$volunteer_notifications <- DT::renderDataTable({
    req(user$type == "volunteer")
    
    joined <- data$care_requests %>%
      left_join(data$elderly_requests, by = c("id" = "request_id")) %>%
      filter(status == "pending") %>%
      left_join(data$elderly, by = c("elderly_id" = "id")) %>%
      mutate(
        match_score = round(runif(n(), 0.65, 0.95), 2),  # Optional simulated score
        Actions = paste0(
          '<button id="accept_', id, '" class="btn btn-success btn-sm">Accept</button>'
        )
      ) %>%
      select(Request_ID = id, Elderly = name, Needs = needs, Distance_km = "5.2", Urgency = urgency, Time = time_prefs, Match_Score = match_score, Actions)
    
    datatable(joined, escape = FALSE, selection = 'none', rownames = FALSE, options = list(dom = 't'))
  })
  
  
  # ----- Launch Elderly Chatbot -----
  observeEvent(input$launch_elderly_bot, {
    showModal(modalDialog(
      title = paste("CareMatch Chatbot —", user$data$name),
      size = "l",
      easyClose = TRUE,
      uiOutput("elderly_chat_ui")
    ))
  })
  
  # Placeholder logic — will be extended
  output$elderly_chat_ui <- renderUI({
    tagList(
      p("Hi there! How can I help?"),
      actionButton("elderly_chat_recommend", "Recommend Volunteer"),
      actionButton("elderly_chat_addreq", "Add New Request")
    )
  })
  
  # ----- Volunteer Chatbot Modal -----
  observeEvent(input$launch_volunteer_bot, {
    showModal(modalDialog(
      title = paste("CareMatch Volunteer Bot —", user$data$name),
      size = "l",
      easyClose = TRUE,
      uiOutput("volunteer_chat_ui")
    ))
  })
  
  observeEvent(input$accept_request_id, {
    rid <- input$accept_request_id
    
    # Append to volunteer_responses
    data$volunteer_responses <- bind_rows(
      data$volunteer_responses,
      data.frame(
        id = paste0("VR_", nrow(data$volunteer_responses) + 1),
        request_id = rid,
        volunteer_id = user$id,
        status = "accepted",
        responded_at = Sys.time()
      )
    )
    showNotification(paste("You accepted request", rid), type = "message")
  })
  
  
  output$volunteer_chat_ui <- renderUI({
    tagList(
      p("Hi! What would you like to do?"),
      actionButton("vol_chat_recommend", "R: Recommendations"),
      actionButton("vol_chat_avail", "S: Set Availability"),
      actionButton("vol_chat_radius", "E: Extend Radius")
    )
  })
  
  # Volunteer Sent Offers (My Sent)
  output$volunteer_sent <- DT::renderDataTable({
    req(user$type == "volunteer")
    
    data$volunteer_responses %>%
      filter(volunteer_id == user$id) %>%
      left_join(data$care_requests, by = c("request_id" = "id")) %>%
      left_join(data$elderly, by = c("elderly_id" = "id")) %>%
      select(request_id, elderly_name = name, needs, time_prefs, urgency, responded_at, status)
  })
  
  # Volunteer Notifications (rule-based matches)
  output$volunteer_notifications <- renderUI({
    req(user$type == "volunteer")
    
    vol <- user$data
    available_slots <- data$availability %>%
      filter(volunteer_id == vol$id) %>%
      pull(time_slot)
    
    potential_requests <- data$care_requests %>%
      left_join(data$elderly_requests, by = c("id" = "request_id")) %>%
      filter(status == "pending") %>%
      mutate(
        elderly_lat = sapply(elderly_id, function(id) data$elderly$lat[data$elderly$id == id]),
        elderly_lng = sapply(elderly_id, function(id) data$elderly$lng[data$elderly$id == id]),
        distance = mapply(calc_distance, vol$lat, vol$lng, elderly_lat, elderly_lng),
        skill_match = sapply(needs, function(n) calc_skill_match(n, vol$skills))
      ) %>%
      filter(
        distance <= vol$radius_km,
        mapply(function(tp) any(strsplit(tp, ", ")[[1]] %in% available_slots), time_prefs)
      ) %>%
      arrange(desc(skill_match))
    
    if (nrow(potential_requests) == 0) return(p("No matching care requests."))
    
    tagList(
      h4("Recommended Care Requests"),
      lapply(1:nrow(potential_requests), function(i) {
        req <- potential_requests[i,]
        box(
          width = 12,
          h5(paste("Needs:", req$needs)),
          p(paste("Urgency:", req$urgency)),
          p(paste("Time:", req$time_prefs)),
          p(paste("Distance:", round(req$distance, 1), "km")),
          p(paste("Skill match:", round(req$skill_match * 100), "%")),
          actionButton(
            paste0("accept_request_", req$id),
            "Accept",
            class = "btn-success btn-sm"
          )
        )
      })
    )
  })
  
  
  output$volunteer_chatbot <- renderUI({
    req(user$type == "volunteer")
    
    tagList(
      h4("🤖 CareMatch Chatbot"),
      p("Hi ", user$data$name, "! How can I assist you today?"),
      radioButtons("vol_bot_choice", "Choose an option:",
                   choices = list(
                     "(R) Recommendations" = "R",
                     "(S) Set Availability" = "S",
                     "(E) Extend Radius" = "E"
                   )),
      actionButton("vol_bot_proceed", "Continue", class = "btn-primary")
    )
  })
  
  observeEvent(input$vol_bot_proceed, {
    if (input$vol_bot_choice == "R") {
      # Top ML-matched requests
      showModal(modalDialog(
        title = "📋 ML-based Recommendations",
        renderUI({
          top_matches <- data$care_requests %>%
            left_join(data$elderly_requests, by = c("id" = "request_id")) %>%
            filter(status == "pending") %>%
            mutate(score = runif(n())) %>% # Placeholder score
            arrange(desc(score)) %>%
            head(10)
          
          tagList(
            lapply(1:nrow(top_matches), function(i) {
              req <- top_matches[i,]
              tags$div(
                h5(paste(i, "-", req$needs)),
                p(paste("Urgency:", req$urgency)),
                p(paste("Time:", req$time_prefs)),
                actionButton(paste0("ml_accept_", req$id), "Accept", class = "btn-sm btn-success")
              )
            })
          )
        }),
        easyClose = TRUE
      ))
    }
    if (input$vol_bot_choice == "S") {
      showModal(modalDialog(
        title = "🕒 Update Availability",
        checkboxGroupInput("new_slots", "Select Time Slots", choices = TIME_SLOTS),
        actionButton("save_availability", "Save"),
        easyClose = TRUE
      ))
    }
    if (input$vol_bot_choice == "E") {
      showModal(modalDialog(
        title = "📍 Extend Radius",
        sliderInput("new_radius", "Service Radius (km)", min = 5, max = 30, value = user$data$radius_km),
        actionButton("update_radius", "Update Radius"),
        easyClose = TRUE
      ))
    }
  })
  
  observeEvent(input$start_elderly_chatbot, {
    showModal(modalDialog(
      title = paste("Hi", user$data$name, "! I'm your CareMatch Bot 👋"),
      size = "l",
      footer = modalButton("Close Chat"),
      easyClose = TRUE,
      uiOutput("elderly_chatbot_flow")
    ))
  })
  
  output$elderly_chatbot_flow <- renderUI({
    req(user$type == "elderly")
    
    # Pull pending requests
    pending_requests <- data$elderly_requests %>%
      filter(elderly_id == user$id, status == "pending") %>%
      left_join(data$care_requests, by = c("request_id" = "id"))
    
    if (nrow(pending_requests) == 0) {
      return(p("You have no pending care requests."))
    }
    
    # Pick the first one for simplicity
    selected_req <- pending_requests[1, ]
    
    # Rule-based matching: get volunteers with matching skills & time
    matches <- data$volunteers %>%
      mutate(skill_match = sapply(skills, function(s) calc_skill_match(selected_req$needs, s))) %>%
      inner_join(data$availability %>%
                   filter(time_slot %in% strsplit(selected_req$time_prefs, ", ")[[1]]),
                 by = c("id" = "volunteer_id")) %>%
      filter(skill_match > 0) %>%
      arrange(desc(skill_match), desc(response_rate)) %>%
      head(3)
    
    if (nrow(matches) == 0) {
      return(p("Sorry, no matching volunteers right now."))
    }
    
    # Save matches to session for use on button click
    user$chatbot_matches <- matches
    user$selected_request_id <- selected_req$request_id
    
    tagList(
      h4("Matching Volunteers for Your Request:"),
      HTML(paste0("<ol>", paste0(sprintf(
        "<li>%s (Skills: %s, Response Rate: %s, Time: %s)</li>",
        matches$name, matches$skills, matches$response_rate,
        sapply(matches$id, function(v) {
          paste(data$availability$time_slot[data$availability$volunteer_id == v], collapse = ", ")
        })
      ), collapse = ""), "</ol>")),
      textInput("elderly_chatbot_choice", "Who would you like to contact? (1/2/3/all)", ""),
      actionButton("elderly_send_request", "Send Request 💌")
    )
  })
  
  observeEvent(input$elderly_send_request, {
    choice <- tolower(trimws(input$elderly_chatbot_choice))
    selected <- user$chatbot_matches
    
    if (choice %in% c("all", "1", "2", "3")) {
      if (choice == "all") {
        selected_ids <- selected$id
      } else {
        selected_ids <- selected$id[as.integer(choice)]
      }
      
      # Update elderly_requests
      idx <- which(data$elderly_requests$request_id == user$selected_request_id)
      data$elderly_requests$requested_volunteers[idx] <- paste(selected_ids, collapse = ",")
      showNotification(paste("Request sent to", paste(selected_ids, collapse = ", ")), type = "message")
      removeModal()
    } else {
      showNotification("Invalid choice. Type 1, 2, 3 or all.", type = "error")
    }
  })
  
  observeEvent(input$start_volunteer_chatbot, {
    showModal(modalDialog(
      title = paste("Hi", user$data$name, "! I'm your CareMatch Assistant 🤖"),
      size = "l",
      footer = modalButton("Close Chat"),
      easyClose = TRUE,
      uiOutput("volunteer_chatbot_flow")
    ))
  })
  
  output$volunteer_chatbot_flow <- renderUI({
    req(user$type == "volunteer")
    
    # Recommend top 10 care requests based on ML score
    scored_requests <- data$care_requests %>%
      left_join(data$elderly_requests %>% filter(status == "pending"), by = c("id" = "request_id")) %>%
      filter(!is.na(request_id)) %>%
      mutate(
        distance = calc_distance(
          data$volunteers$lat[data$volunteers$id == user$id],
          data$volunteers$lng[data$volunteers$id == user$id],
          data$elderly$lat[match(elderly_id, data$elderly$id)],
          data$elderly$lng[match(elderly_id, data$elderly$id)]
        ),
        skill_match = sapply(needs, function(n) {
          vol_skills <- data$volunteers$skills[data$volunteers$id == user$id]
          calc_skill_match(n, vol_skills)
        }),
        time_match = sapply(time_prefs, function(tp) {
          slot_list <- strsplit(tp, ", ")[[1]]
          user_slots <- data$availability$time_slot[data$availability$volunteer_id == user$id]
          as.integer(length(intersect(slot_list, user_slots)) > 0)
        }),
        score = 0.4 * user$data$response_rate +
          0.3 * (1 - distance/15) +
          0.3 * skill_match
      ) %>%
      arrange(desc(score)) %>%
      head(10)
    
    if (nrow(scored_requests) == 0) {
      return(p("No recommended requests available."))
    }
    
    tagList(
      h4("Recommended Requests for You:"),
      HTML(paste0("<ol>", paste0(sprintf(
        "<li>%s (%.1f km, %s urgency, %s)</li>",
        scored_requests$needs, scored_requests$distance, scored_requests$urgency, scored_requests$time_prefs
      ), collapse = ""), "</ol>"))
    )
  })
  
  observe({
    session$sendCustomMessage("acceptHandler", list())
  })


  output$elderly_offers_ui <- renderUI({
  req(user$type == "elderly")
  pending_reqs <- data$elderly_requests %>%
    filter(elderly_id == user$id, status == "pending") %>%
    left_join(data$care_requests, by = c("request_id" = "id"))
  
  if (nrow(pending_reqs) == 0) {
    return(p("No pending requests to match."))
  }
  
  volunteer_list <- lapply(1:nrow(pending_reqs), function(i) {
    req_row <- pending_reqs[i, ]
    time_pref <- strsplit(req_row$time_prefs, ", ")[[1]]
    
    potential_vols <- data$volunteers %>%
      inner_join(data$availability %>% filter(time_slot %in% time_pref),
                 by = c("id" = "volunteer_id")) %>%
      mutate(
        skill_match = sapply(skills, function(s) calc_skill_match(req_row$needs, s)),
        score = 0.4 * response_rate + 0.3 * skill_match +
          0.3 * runif(n())  # Replace with normalized distance later
      ) %>%
      arrange(desc(score)) %>%
      head(5)
    
    if (nrow(potential_vols) == 0) return(NULL)
    
    tagList(
      h4(paste("Request:", req_row$needs)),
      lapply(1:nrow(potential_vols), function(j) {
        v <- potential_vols[j, ]
        fluidRow(
          column(10,
                 tags$b(v$id), br(),
                 p(paste("Skills:", v$skills)),
                 p(paste("Time Prefs:", v$time_prefs)),
                 p(paste("Response Rate:", round(v$response_rate * 100), "%"))
          ),
          column(2,
                 actionButton(
                   inputId = paste0("accept_", req_row$request_id, "_", v$id),
                   label = "Accept",
                   class = "btn-primary btn-sm"
                 )
          )
        )
      })
    )
  })
  
  do.call(tagList, volunteer_list)
})
}

shinyApp(ui, server)
