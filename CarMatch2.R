library(shiny)
library(dplyr)
library(lubridate)

# Sample data frames
elderly_profiles <- data.frame(
  name = c("John Doe", "Jane Smith"),
  age = c(75, 82),
  gender = c("Male", "Female"),
  location = c("New York", "Los Angeles"),
  needs = c("Grocery help, Transportation", "Companionship, Meal prep"),
  stringsAsFactors = FALSE
)

volunteer_profiles <- data.frame(
  name = c("Alice Brown", "Bob Wilson"),
  skills = c("Grocery help, Transportation", "Companionship, Housekeeping"),
  location = c("New York", "Los Angeles"),
  availability = c("Mon 10-12, Wed 14-16", "Tue 09-11, Thu 13-15"),
  stringsAsFactors = FALSE
)

ui <- fluidPage(
  titlePanel("CareMatch - Elderly Care Matching System"),
  sidebarLayout(
    sidebarPanel(
      textInput("user_input", "Type your response:"),
      actionButton("submit", "Submit"),
      br(),
      helpText("Conversation flow: 1. Name -> 2. Age -> 3. Gender -> 4. Location -> 5. Needs")
    ),
    mainPanel(
      uiOutput("chat_history"),
      br(),
      uiOutput("recommendations")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    step = 1,
    user_data = list(),
    selected_needs = character()
  )
  
  observeEvent(input$submit, {
    if (rv$step == 1) {
      # Find user profile
      user_profile <- elderly_profiles %>%
        filter(tolower(name) == tolower(input$user_input))
      
      if (nrow(user_profile) > 0) {
        rv$user_data <- as.list(user_profile)
        rv$selected_needs <- unlist(strsplit(user_profile$needs, ", "))
        rv$step <- 5  # Skip to needs confirmation
      } else {
        rv$user_data$name <- input$user_input
        rv$step <- 2
      }
    } else if (rv$step == 2) {
      rv$user_data$age <- input$user_input
      rv$step <- 3
    } else if (rv$step == 3) {
      rv$user_data$gender <- input$user_input
      rv$step <- 4
    } else if (rv$step == 4) {
      rv$user_data$location <- input$user_input
      rv$step <- 5
    } else if (rv$step == 5) {
      if (length(rv$selected_needs) == 0) {
        # If no profile found, ask to select needs
        rv$selected_needs <- unlist(strsplit(input$user_input, ", "))
      }
      rv$step <- 6
    }
    
    # Clear input after submission
    updateTextInput(session, "user_input", value = "")
  })
  
  output$chat_history <- renderUI({
    tagList(
      if (rv$step >= 1) p(strong("System:"), "Please enter your full name"),
      if (rv$step >= 2) p(strong("System:"), "Please enter your age"),
      if (rv$step >= 3) p(strong("System:"), "Please enter your gender"),
      if (rv$step >= 4) p(strong("System:"), "Please enter your location (city/state)"),
      if (rv$step >= 5 && length(rv$selected_needs) == 0) {
        needs <- unique(unlist(strsplit(elderly_profiles$needs, ", ")))
        p(strong("System:"), "Please select your needs from: ", paste(needs, collapse = ", "))
      }
    )
  })
  
  output$recommendations <- renderUI({
    if (rv$step == 6) {
      # Matching algorithm
      matched_volunteers <- volunteer_profiles %>%
        filter(tolower(location) == tolower(rv$user_data$location)) %>%
        mutate(
          skills_list = strsplit(skills, ", "),
          jaccard = sapply(skills_list, function(x) {
            intersection <- length(intersect(x, rv$selected_needs))
            union <- length(union(x, rv$selected_needs))
            intersection / union
          })
        ) %>%
        filter(jaccard > 0) %>%
        arrange(desc(jaccard))
      
      tagList(
        h4("Recommended Volunteers:"),
        if (nrow(matched_volunteers) > 0) {
          renderTable({
            matched_volunteers %>%
              select(Name = name, Skills = skills, Location = location, 
                     Availability = availability, Match_Score = jaccard)
          })
        } else {
          p("No matching volunteers found. Please try different needs or check back later.")
        }
      )
    }
  })
}

shinyApp(ui, server)
