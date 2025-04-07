library(shiny)
library(zipcodeR)
library(dplyr)

# Initialize elderly.csv with correct structure if not exists
if(!file.exists("elderly.csv")) {
  data.frame(
    elderly_id = integer(),
    name = character(),
    age = integer(),
    major_city = character(),
    state = character(),
    lat = numeric(),
    lng = numeric(),
    preferred_time_slots = character(),
    stringsAsFactors = FALSE
  ) %>% write.csv("elderly.csv", row.names = FALSE)
}

if(!file.exists("volunteers.csv")) {
  data.frame(
    volunteer_id = integer(),
    volunteer_name = character(),
    major_city = character(),
    state = character(),
    lat = numeric(),
    lng = numeric(),
    availability = character(),
    radius = integer(),
    acceptance_probability = numeric(),
    stringsAsFactors = FALSE
  ) %>% write.csv("volunteers.csv", row.names = FALSE)
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      #chatbox {
        height: 400px;
        overflow-y: scroll;
        border: 1px solid #ccc;
        padding: 10px;
        margin-bottom: 10px;
        background-color: #f9f9f9;
      }
      .user-msg { 
        text-align: right; 
        color: blue;
        margin: 5px;
      }
      .bot-msg { 
        text-align: left; 
        color: green;
        margin: 5px;
      }
    "))
  ),
  titlePanel("CareMatch Assistant Chatbot"),
  mainPanel(
    width = 12,
    wellPanel(
      id = "chatbox",
      uiOutput("chatHistory")
    ),
    fluidRow(
      column(10, textInput("userInput", "", placeholder = "Type your response here...")),
      column(2, actionButton("submit", "Send", class = "btn-primary"))
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    chat_history = list(),
    current_state = "ask_type",
    registration_data = list(),
    current_question = NULL
  )
  
  # Initial bot message
  observe({
    if(length(rv$chat_history) == 0) {
      bot_message("Welcome! Are you an 'Elderly person (seeking assistance)' or 'Volunteer'?")
    }
  })
  
  bot_message <- function(text) {
    rv$chat_history <- c(rv$chat_history, list(list(
      type = "bot",
      text = text
    )))
  }
  
  user_message <- function(text) {
    rv$chat_history <- c(rv$chat_history, list(list(
      type = "user",
      text = text
    )))
  }
  
  output$chatHistory <- renderUI({
    tagList(
      lapply(rv$chat_history, function(msg) {
        div(class = paste0(msg$type, "-msg"), 
            strong(ifelse(msg$type == "user", "You: ", "Bot: ")),
            msg$text)
      })
    )
  })
  
  observeEvent(input$submit, {
    req(input$userInput)
    user_input <- trimws(input$userInput)
    if(user_input == "") return()
    
    user_message(user_input)
    
    if(rv$current_state == "ask_type") {
      handle_initial_input(user_input)
    } else if(rv$current_state == "elderly_registration_check") {
      handle_elderly_registration_check(user_input)
    } else if(rv$current_state == "elderly_registration") {
      handle_elderly_registration(user_input)
    } else if(rv$current_state == "volunteer_registration_check") {
      handle_volunteer_registration_check(user_input)
    } else if(rv$current_state == "volunteer_registration") {
      handle_volunteer_registration(user_input)
    }
    
    updateTextInput(session, "userInput", value = "")
  })
  
  handle_initial_input <- function(input) {
    if(tolower(input) %in% c("elderly", "elderly person", "elderly person seeking assistance")) {
      bot_message("Thank you for letting us know you're seeking assistance. Have you registered with us before? (Yes/No)")
      rv$current_state <- "elderly_registration_check"
    } else if(tolower(input) %in% c("volunteer", "v")) {
      bot_message("Thank you for volunteering!Have you registered with us before? (Yes/No)")
      rv$current_state <- "volunteer_registration_check"
      rv$user_type <- "volunteer"
    } else {
      bot_message("Please specify either 'Elderly person seeking assistance' or 'Volunteer'")
    }
  }
  
  handle_volunteer_registration_check <- function(input) {
    if(tolower(input) %in% c("yes", "y")) {
      bot_message("Thank you for your service! We'll help you find opportunities.")
      rv$current_state <- "volunteer_ready"
    } else if(tolower(input) %in% c("no", "n")) {
      start_volunteer_registration()
    } else {
      bot_message("Please answer with 'Yes' or 'No'")
    }
  }
  
  start_volunteer_registration <- function() {
    bot_message("Let's register you as a volunteer. Please enter your full name (Last name, First name):")
    rv$current_state <- "volunteer_registration"
    rv$current_question <- "vol_name"
  }
  
  handle_volunteer_registration <- function(input) {
    current_q <- rv$current_question
    
    switch(current_q,
           "vol_name" = process_vol_name(input),
           "vol_city" = process_vol_city(input),
           "vol_state" = process_vol_state(input),
           "vol_zip" = process_vol_zip(input),
           "vol_availability" = process_vol_availability(input)
    )
  }
  
  process_vol_name <- function(input) {
    if(!grepl(",", input)) {
      bot_message("Please enter your name in 'Last name, First name' format")
      return()
    }
    rv$registration_data$volunteer_name <- input
    bot_message("Which major city do you operate in?")
    rv$current_question <- "vol_city"
  }
  
  process_vol_city <- function(input) {
    rv$registration_data$major_city <- input
    bot_message("Which state? (2-letter abbreviation)")
    rv$current_question <- "vol_state"
  }
  
  process_vol_state <- function(input) {
    if(!grepl("^[A-Za-z]{2}$", input)) {
      bot_message("Please use 2-letter state abbreviation (e.g., CA, NY)")
      return()
    }
    rv$registration_data$state <- toupper(input)
    bot_message("What's your 5-digit zipcode?")
    rv$current_question <- "vol_zip"
  }
  
  process_vol_zip <- function(input) {
    if(!grepl("^\\d{5}$", input)) {
      bot_message("Please enter a valid 5-digit zipcode")
      return()
    }
    
    coords <- geocode_zip(input)
    if(is.null(coords)) {
      bot_message("Could not find coordinates for this zipcode. Please try again.")
      return()
    }
    
    rv$registration_data$lat <- coords$lat
    rv$registration_data$lng <- coords$lng
    bot_message("Please enter your availability (e.g., 'Mon 9-12, Wed 3-6')")
    rv$current_question <- "vol_availability"
  }
  
  process_vol_availability <- function(input) {
    if(!grepl("[A-Za-z]{3} \\d+-\\d+", input)) {
      bot_message("Please use format like 'Mon 9-12, Wed 3-6'")
      return()
    }
    
    rv$registration_data$availability <- input
    save_volunteer_registration()
    bot_message("Volunteer registration complete! Thank you for your generosity.")
    rv$current_state <- "volunteer_ready"
  }
  
  save_volunteer_registration <- function() {
    existing_data <- read.csv("volunteers.csv")
    new_id <- ifelse(nrow(existing_data) == 0, 1, max(existing_data$volunteer_id) + 1)
    
    new_entry <- data.frame(
      volunteer_id = new_id,
      volunteer_name = rv$registration_data$volunteer_name,
      major_city = rv$registration_data$major_city,
      state = rv$registration_data$state,
      lat = rv$registration_data$lat,
      lng = rv$registration_data$lng,
      availability = rv$registration_data$availability,
      radius = NA,
      acceptance_probability = NA
    )
    
    write.table(new_entry, "volunteers.csv",
                append = TRUE, sep = ",",
                row.names = FALSE, col.names = FALSE)
  }
  
  
  
  
  
  
  
  
  
  
  
  handle_registration_check <- function(input) {
    if(tolower(input) %in% c("yes", "y")) {
      bot_message("Please wait while we connect you with a volunteer...")
      rv$current_state <- "elderly_waiting"
    } else if(tolower(input) %in% c("no", "n")) {
      start_registration()
    } else {
      bot_message("Please answer with 'Yes' or 'No'")
    }
  }
  
  start_registration <- function() {
    bot_message("Let's register you. Please enter your full name (Last name, First name):")
    rv$current_state <- "elderly_registration"
    rv$current_question <- "name"
  }
  
  handle_registration_process <- function(input) {
    current_q <- rv$current_question
    
    switch(current_q,
           "name" = process_name(input),
           "age" = process_age(input),
           "city" = process_city(input),
           "state" = process_state(input),
           "zipcode" = process_zipcode(input),
           "time_slots" = process_time_slots(input)
    )
  }
  
  process_name <- function(input) {
    if(!grepl(",", input)) {
      bot_message("Please enter your name in 'Last name, First name' format")
      return()
    }
    rv$registration_data$name <- input
    bot_message("What is your age?")
    rv$current_question <- "age"
  }
  
  process_age <- function(input) {
    if(!grepl("^\\d+$", input) | as.numeric(input) < 65) {
      bot_message("Please enter a valid age (65 or older)")
      return()
    }
    rv$registration_data$age <- as.integer(input)
    bot_message("Which major city do you live in?")
    rv$current_question <- "city"
  }
  
  process_city <- function(input) {
    rv$registration_data$major_city <- input
    bot_message("Which state? (2-letter abbreviation)")
    rv$current_question <- "state"
  }
  
  process_state <- function(input) {
    if(!grepl("^[A-Za-z]{2}$", input)) {
      bot_message("Please use 2-letter state abbreviation (e.g., CA, NY)")
      return()
    }
    rv$registration_data$state <- toupper(input)
    bot_message("What's your 5-digit zipcode?")
    rv$current_question <- "zipcode"
  }
  
  process_zipcode <- function(input) {
    if(!grepl("^\\d{5}$", input)) {
      bot_message("Please enter a valid 5-digit zipcode")
      return()
    }
    
    coords <- geocode_zip(input)
    if(is.null(coords)) {
      bot_message("Could not find coordinates for this zipcode. Please try again.")
      return()
    }
    
    rv$registration_data$lat <- coords$lat
    rv$registration_data$lng <- coords$lng
    bot_message("Please enter your preferred time slots (e.g., 'Mon 9-12, Wed 3-6')")
    rv$current_question <- "time_slots"
  }
  
  process_time_slots <- function(input) {
    if(!grepl("[A-Za-z]{3} \\d+-\\d+", input)) {
      bot_message("Please use format like 'Mon 9-12, Wed 3-6'")
      return()
    }
    
    rv$registration_data$preferred_time_slots <- input
    save_registration()
    bot_message("Registration complete! We'll connect you with a volunteer shortly.")
    rv$current_state <- "elderly_waiting"
  }
  
  save_registration <- function() {
    existing_data <- read.csv("elderly.csv")
    new_id <- ifelse(nrow(existing_data) == 0, 1, max(existing_data$elderly_id) + 1)
    
    new_entry <- data.frame(
      elderly_id = new_id,
      name = rv$registration_data$name,
      age = rv$registration_data$age,
      major_city = rv$registration_data$major_city,
      state = rv$registration_data$state,
      lat = rv$registration_data$lat,
      lng = rv$registration_data$lng,
      preferred_time_slots = rv$registration_data$preferred_time_slots
    )
    
    write.table(new_entry, "elderly.csv",
                append = TRUE, sep = ",",
                row.names = FALSE, col.names = FALSE)
  }
}

geocode_zip <- function(zipcode) {
  result <- reverse_zipcode(zipcode)
  if(nrow(result) == 0) return(NULL)
  data.frame(
    lat = result$lat[1],
    lng = result$lng[1]
  )
}

shinyApp(ui, server, options = list(host = "0.0.0.0", port = 7698))
