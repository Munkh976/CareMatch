library(shiny)

# Set the working directory
setwd("C:/MS Data Science - WMU/CS5610-Advanced R/Projects/CareMatch/Project Code/CareMatch")

# Load data from CSV
elderly_profiles <- read.csv("elderly.csv", stringsAsFactors = FALSE)
volunteer_profiles <- read.csv("volunteers.csv", stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Elderly Care Matching Chatbot"),
  sidebarLayout(
    sidebarPanel(
      selectInput("user_type", "Are you an Elderly Person or a Volunteer?", 
                  choices = c("Elderly", "Volunteer")),
      textInput("last_name", "Your Last Name:"),
      numericInput("user_age", "Your Age:", value = NA, min = 60, max = 120),
      selectInput("gender", "Your Gender:", choices = c("Male", "Female")),
      textInput("user_location", "Your Zip Code:"),
      conditionalPanel(
        condition = "input.user_type == 'Elderly'",
        uiOutput("needs_choices"),
        selectInput("preferred_time", "Preferred Time:", 
                    choices = c("Morning", "Afternoon", "Evening")),
        actionButton("submit_elderly", "Find Volunteers")
      ),
      width = 4
    ),
    mainPanel(
      uiOutput("chat"),
      width = 8
    )
  )
)

# Define Server
server <- function(input, output, session) {
  chat_history <- reactiveVal(data.frame(User = character(), Message = character(), stringsAsFactors = FALSE))
  
  observeEvent(input$last_name, {
    matched_elder <- elderly_profiles[elderly_profiles$Last_Name == input$last_name & 
                                        elderly_profiles$Age == input$user_age & 
                                        elderly_profiles$Gender == input$gender & 
                                        elderly_profiles$Location == input$user_location, ]
    
    if (nrow(matched_elder) > 0) {
      updateCheckboxGroupInput(session, "user_needs", "Select Your Needs:", 
                               choices = unique(unlist(strsplit(matched_elder$Needs, ", "))),
                               selected = unlist(strsplit(matched_elder$Needs, ", ")))
    }
  })
  
  output$needs_choices <- renderUI({
    checkboxGroupInput("user_needs", "Select Your Needs:", 
                       choices = unique(unlist(strsplit(paste(elderly_profiles$Needs, collapse = ", "), ", "))))
  })
  
  observeEvent(input$submit_elderly, {
    req(input$last_name, input$user_age, input$gender, input$user_location, input$user_needs, input$preferred_time)
    isolate({
      new_entry <- data.frame(
        User = input$last_name, 
        Message = paste("Hello", input$last_name, "! You need help with:", paste(input$user_needs, collapse = ", "), 
                        "at", input$preferred_time, "."), 
        stringsAsFactors = FALSE
      )
      chat_history(rbind(chat_history(), new_entry))
      
      # Matching Volunteers
      matched_volunteers <- volunteer_profiles[volunteer_profiles$Location == input$user_location & 
                                                 grepl(paste(input$user_needs, collapse = "|"), volunteer_profiles$Skills) & 
                                                 volunteer_profiles$Availability == input$preferred_time, ]
      
      if (nrow(matched_volunteers) > 0) {
        matches <- paste(matched_volunteers$Name, "- Skills:", matched_volunteers$Skills, "- Available:", matched_volunteers$Availability, collapse = "\n")
      } else {
        matches <- "No matching volunteers found."
      }
      
      chat_history(rbind(chat_history(), data.frame(User = "System", Message = matches, stringsAsFactors = FALSE)))
    })
  })
  
  output$chat <- renderUI({
    chat_content <- chat_history()
    if (nrow(chat_content) == 0) {
      h4("Welcome! Please enter your details to find assistance.")
    } else {
      do.call(tagList, lapply(1:nrow(chat_content), function(i) {
        div(
          strong(chat_content$User[i]), ": ", chat_content$Message[i],
          style = "margin-bottom: 10px;"
        )
      }))
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)

