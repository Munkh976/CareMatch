library(shiny)
setwd("C:/MS Data Science - WMU/CS5610-Advanced R/Projects/CareMatch/Project Code")
# Generate sample data for 1000 elderly users and 1000 volunteers and save to CSV
set.seed(123)
elderly_profiles <- data.frame(
  Last_Name = paste0("Elder", 1:1000),
  Age = sample(60:100, 1000, replace = TRUE),
  Gender = sample(c("Male", "Female"), 1000, replace = TRUE),
  Location = sample(10000:10999, 1000, replace = TRUE),
  Needs = sample(c("Walking Assistance", "Grocery Shopping", "House Cleaning", "Meal Preparation"), 1000, replace = TRUE),
  Preferred_Time = sample(c("Morning", "Afternoon", "Evening"), 1000, replace = TRUE),
  stringsAsFactors = FALSE
)

volunteer_profiles <- data.frame(
  Name = paste0("Volunteer", 1:1000),
  Location = sample(10000:10999, 1000, replace = TRUE),
  Skills = sample(c("Walking Assistance", "Grocery Shopping", "House Cleaning", "Meal Preparation"), 1000, replace = TRUE),
  Availability = sample(c("Morning", "Afternoon", "Evening"), 1000, replace = TRUE),
  stringsAsFactors = FALSE
)

write.csv(elderly_profiles, "elderly_profiles.csv", row.names = FALSE)
write.csv(volunteer_profiles, "volunteer_profiles.csv", row.names = FALSE)

# Load data from CSV
elderly_profiles <- read.csv("elderly_profiles.csv", stringsAsFactors = FALSE)
volunteer_profiles <- read.csv("volunteer_profiles.csv", stringsAsFactors = FALSE)

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
                               choices = unique(elderly_profiles$Needs),
                               selected = matched_elder$Needs)
    }
  })
  
  output$needs_choices <- renderUI({
    checkboxGroupInput("user_needs", "Select Your Needs:", 
                       choices = unique(elderly_profiles$Needs))
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
                                                 volunteer_profiles$Skills %in% input$user_needs &
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
