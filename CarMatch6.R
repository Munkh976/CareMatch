library(shiny)
library(dplyr)
library(tidyr)
library(randomNames)
library(zipcodeR)
library(lubridate)

# Load data
setwd("C:/MS Data Science - WMU/CS5610-Advanced R/Projects/CareMatch/Project Code")

elderly_profiles <- read.csv("elderly_profiles.csv", stringsAsFactors = FALSE)
volunteer_profiles <- read.csv("volunteer_profiles.csv", stringsAsFactors = FALSE)
care_requests <- read.csv("care_requests.csv", stringsAsFactors = FALSE)
notifications <- read.csv("notifications.csv", stringsAsFactors = FALSE)

ui <- fluidPage(
  titlePanel("CareMatch Community Portal"),
  tags$head(tags$style("
    .error {color: red; font-weight: bold;}
    .warning {color: orange;}
  ")),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "user-type",
          radioButtons("user_type", "Are you here as:",
                       choices = c("Elder needing care" = "elder",
                                   "Volunteer caregiver" = "volunteer"),
                       selected = character(0))
      ),
      numericInput("user_id", "Enter your ID:", value = NA, min = 1),
      actionButton("submit", "Submit", class = "btn-primary"),
      uiOutput("login_error"),
      width = 3
    ),
    mainPanel(
      uiOutput("main_display"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    current_step = "type_selection",
    user_data = NULL,
    login_error = NULL
  )
  
  observeEvent(input$user_type, {
    rv$current_step <- "auth"
    rv$user_data <- list(type = input$user_type)
    rv$login_error <- NULL
  })
  
  output$login_error <- renderUI({
    if (!is.null(rv$login_error)) {
      div(class = "error", rv$login_error)
    }
  })
  
  observeEvent(input$submit, {
    req(input$user_id, input$user_type)
    
    if (is.na(input$user_id)) {
      rv$login_error <- "Please enter a valid ID number"
      return()
    }
    
    user <- switch(
      input$user_type,
      "elder" = elderly_profiles %>% filter(elderly_id == input$user_id),
      "volunteer" = volunteer_profiles %>% filter(volunteer_id == input$user_id),
      {
        rv$login_error <- "Please select user type"
        return()
      }
    )
    
    if (nrow(user) == 0) {
      rv$login_error <- "ID not found in our system"
      return()
    }
    
    rv$user_data <- as.list(user[1, ])
    rv$current_step <- ifelse(input$user_type == "elder", "confirm_needs", "volunteer_dashboard")
    rv$login_error <- NULL
  })
  
  output$main_display <- renderUI({
    req(rv$current_step)
    
    switch(
      rv$current_step,
      "confirm_needs" = confirm_needs_ui(),
      "volunteer_dashboard" = volunteer_dashboard_ui(),
      h4("Please authenticate using your ID")
    )
  })
  
  # Reactive location display:
  output$user_location <- renderUI({
    if (!is.null(rv$user_data)) {  # Check if user data is loaded
      p(strong("Location:"), paste(rv$user_data$city, rv$user_data$state))
    } else {
      NULL # Or some placeholder if you prefer
    }
  })
  
  confirm_needs_ui <- function() {
    tagList(
      h3(paste("Welcome,", rv$user_data$name)),
      uiOutput("user_location"), # Use the reactive output
      h4("Your Current Needs:"),
      p(rv$user_data$needs),
      actionButton("update_needs", "Update Needs", class = "btn-warning"),
      actionButton("submit_request", "Submit New Request", class = "btn-success")
    )
  }
  
  volunteer_dashboard_ui <- function() {
    tagList(
      h3(paste("Welcome,", rv$user_data$name)),
      uiOutput("user_location"), # Use the reactive output
      h4("Your Skills:"),
      p(rv$user_data$skills),
      h4("Pending Requests in Your Area:"),
      uiOutput("volunteer_requests")
    )
  }
  
  
  output$volunteer_requests <- renderUI({
    req(rv$user_data$city, rv$user_data$state) # Require both city and state
    
    requests <- care_requests %>%
      filter(city == rv$user_data$city,
            # state == rv$user_data$state, # Filter by state as well
             status == "Pending") %>%
      left_join(elderly_profiles, by = "elderly_id")
    
    if (nrow(requests) > 0) {
      lapply(1:nrow(requests), function(i) {
        req <- requests[i, ]
        div(class = "request-card",
            h5(paste("Request from", req$name.y)), # Use elderly name from join
            p(paste("Needs:", req$needs.x)), # Use elderly needs
            p(paste("Request Date:", req$request_date)),
            actionButton(paste0("accept_", req$request_id), "Accept",
                         class = "btn-success btn-sm")
        )
      })
    } else {
      h4("No pending requests in your area")
    }
  })
  
  observeEvent(input$submit_request, {
    new_request <- data.frame(
      elderly_id = rv$user_data$elderly_id,
      request_date = Sys.Date(),
      status = "Pending",
      needs = rv$user_data$needs,
      city = rv$user_data$city,
      state = rv$user_data$state
    )
    
    care_requests <<- bind_rows(care_requests, new_request)
    write.csv(care_requests, "care_requests.csv", row.names = FALSE)
    showNotification("New care request submitted!", type = "message")
  })
}

shinyApp(ui, server)

