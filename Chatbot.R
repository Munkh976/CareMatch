library(shiny)
library(zipcodeR)
library(dplyr)
#
setwd("C:/MS Data Science - WMU/CS5610-Advanced R/Projects/Final Project")
# Initialize elderly.csv with correct structure if not exists
if(!file.exists("data/elderly.csv")) {
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

if(!file.exists("data/volunteers.csv")) {
  data.frame(
    volunteer_id = integer(),
    name = character(),
    major_city = character(),
    state = character(),
    lat = numeric(),
    lng = numeric(),
    availability = character(),
    radius_willing = integer(),
    acceptance_probability = numeric(),
    stringsAsFactors = FALSE
  ) %>% write.csv("volunteers.csv", row.names = FALSE)
}

#Intreactive chatbot code #################################
# install.packages("docxtractr")
# install.packages("stringdist")
# ---------------------------
library(docxtractr)
library(stringdist)

# 1. Read your Word file with Q&A
doc <- read_docx("Questions.docx")   # <-- adjust the path/filename if needed
faq_table <- docx_extract_all_tbls(doc)[[1]]

# 2. Rename columns if they came in as V1, V2
names(faq_table) <- c("Question", "Answer")

# 3. Clean up the Question text for matching
faq_table$Question_clean <- tolower(gsub("[^[:alnum:]\\s]+", " ", faq_table$Question))
faq_table$Question_clean <- trimws(faq_table$Question_clean)

# 4. Define a helper function to retrieve the best-matching FAQ answer
get_faq_answer <- function(user_input) {
  # Clean the user’s query
  user_clean <- tolower(gsub("[^[:alnum:]\\s]+", " ", user_input))
  user_clean <- trimws(user_clean)
  
  # Compute similarity to each stored question
  sims <- stringsim(user_clean, faq_table$Question_clean, method = "cosine")
  best_index <- which.max(sims)
  best_score <- sims[best_index]
  
  # If the similarity is very low, fallback
  if(best_score < 0.1) {
    return("I'm not sure how to answer that question, sorry! Can you rephrase?")
  } else {
    return(faq_table$Answer[best_index])
  }
}
##########################

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
      column(2, actionButton("submit", "Send", class = "btn-primary")),
      column(12, actionButton("reset", "Reset Chat", class = "btn-danger"))
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    chat_history = list(),
    current_state = "ask_type",
    registration_data = list(),
    current_question = NULL,
    user_type = NULL,
    lookup_data = list()
  )
  
  # Initial bot message
  observe({
    if(length(rv$chat_history) == 0) {
      bot_message("Welcome! Feel free to interact with the ChatBot. Are you an 'Elderly person (seeking assistance)' or 'Volunteer'?")
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
  
  observeEvent(input$reset, {
    rv$chat_history <- list()
    rv$current_state <- "ask_type"
    rv$registration_data <- list()
    rv$current_question <- NULL
    rv$user_type <- NULL
    rv$lookup_data <- list()
    bot_message("Welcome! Feel free to interact with the ChatBot. Are you an 'Elderly person (seeking assistance)' or 'Volunteer'?")
  })
  
  # NEW: handle_faq_mode function
  handle_faq_mode <- function(user_input) {
    # If user types "volunteer" or "elderly" or "register" while in faq_mode,
    # let's jump to the right flow.
    
    # 1. Check for "volunteer"
    if(tolower(user_input) %in% c("volunteer", "v")) {
      bot_message("Thank you for volunteering! Have you registered with us before? (Yes/No)")
      rv$current_state <- "volunteer_registration_check"
      rv$user_type <- "volunteer"
      
      # 2. Check for "elderly"
    } else if(tolower(user_input) %in% c("elderly", "elderly person", "elderly person seeking assistance")) {
      bot_message("Thank you for letting us know you're seeking assistance. Have you registered with us before? (Yes/No)")
      rv$current_state <- "elderly_registration_check"
      
      # 3. Check for "register"
    } else if(grepl("\\bregister\\b", tolower(user_input))) {
      bot_message("Do you want to register as 'Elderly' or 'Volunteer'?")
      rv$current_state <- "ask_type"
      
      # 4. Otherwise, fallback to FAQ answer
    } else {
      bot_message(get_faq_answer(user_input))
      # stay in "faq_mode"
    }
  }
  
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
    } else if(rv$current_state == "elderly_lookup") {
      handle_elderly_lookup(user_input)
    } else if(rv$current_state == "volunteer_lookup") {
      handle_volunteer_lookup(user_input)
    # NEW: if in faq_mode, check for volunteer/elderly/register
    } else if(rv$current_state == "faq_mode") {
      handle_faq_mode(user_input)
    
    }else {
      # NEW CODE: FAQ Fallback
      # If none of the registration flows apply, handle it as a FAQ query
      bot_message(get_faq_answer(user_input))
    }
    
    
    
    updateTextInput(session, "userInput", value = "")
  })
  
  
  # Modified registration check handlers
  handle_elderly_registration_check <- function(input) {
    if(tolower(input) %in% c("yes", "y")) {
      bot_message("Please provide your registered details. Start with your full name (Last name, First name):")
      rv$current_state <- "elderly_lookup"
      rv$current_question <- "verify_name"
    } else if(tolower(input) %in% c("no", "n")) {
      start_registration()
    } else {
      bot_message("Please answer with 'Yes' or 'No'")
    }
  }
  
  handle_volunteer_registration_check <- function(input) {
    if(tolower(input) %in% c("yes", "y")) {
      bot_message("Please provide your registered details. Start with your full name (Last name, First name):")
      rv$current_state <- "volunteer_lookup"
      rv$current_question <- "verify_name"
    } else if(tolower(input) %in% c("no", "n")) {
      start_volunteer_registration()
    } else {
      bot_message("Please answer with 'Yes' or 'No'")
    }
  }
  
  # Elderly verification flow
  handle_elderly_lookup <- function(input) {
    switch(rv$current_question,
           "verify_name" = process_verify_elderly_name(input),
           "verify_age" = process_verify_elderly_age(input),
           "verify_city" = process_verify_elderly_city(input)
    )
  }
  
  process_verify_elderly_name <- function(input) {
    if(!grepl(",", input)) {
      bot_message("Please enter your name in 'Last name, First name' format")
      return()
    }
    rv$lookup_data$name <- input
    bot_message("What is your registered age?")
    rv$current_question <- "verify_age"
  }
  
  process_verify_elderly_age <- function(input) {
    if(!grepl("^\\d+$", input)) {
      bot_message("Please enter a valid age (numbers only)")
      return()
    }
    rv$lookup_data$age <- as.integer(input)
    bot_message("Which city are you registered in?")
    rv$current_question <- "verify_city"
  }
  
  # Modify the elderly verification section
  process_verify_elderly_city <- function(input) {
    rv$lookup_data$city <- input
    elderly_data <- read.csv("elderly.csv") %>%
      filter(tolower(name) == tolower(rv$lookup_data$name),
             age == rv$lookup_data$age,
             tolower(major_city) == tolower(rv$lookup_data$city))
    
    if(nrow(elderly_data) == 0) {
      bot_message("No matching registration found. Would you like to register instead? (Yes/No)")
      rv$current_state <- "elderly_registration_retry"
    } else {
      bot_message("Here are your registration details:")
      bot_message(paste(
        paste("Name:", elderly_data$name),
        paste("Age:", elderly_data$age),
        paste("City:", elderly_data$major_city),
        paste("State:", elderly_data$state),
        paste("Preferred Times:", elderly_data$preferred_time_slots),
        sep = "\n"
      ))
      
      rv$current_elderly_id <- elderly_data$elderly_id[1]
      
      if(file.exists("updated_matches.csv")) {
        matches <- read.csv("updated_matches.csv") %>%
          filter(elderly_id == rv$current_elderly_id)
        
        if(nrow(matches) > 0) {
          volunteers <- read.csv("volunteers.csv") %>%
            filter(volunteer_id %in% matches$volunteer_id)
          
          if(nrow(volunteers) > 0) {
            bot_message("\n\nYour matched volunteer(s):")
            lapply(1:nrow(volunteers), function(i) {
              bot_message(paste(
                paste("Volunteer Name:", volunteers$volunteer_name[i]),
                paste("Location:", volunteers$major_city[i], volunteers$state[i]),
                paste("Availability:", volunteers$availability[i]),
                sep = "\n"
              ))
            })
          } else {
            bot_message("\nNo volunteers found for your matches. Please check back later.")
          }
        } else {
          bot_message("\nNo current matches found. We'll notify you when we find a match.")
        }
      } else {
        bot_message("\nMatch system is currently unavailable. Please check back later.")
      }
      
      rv$current_state <- "elderly_waiting"
    }
  }
  
  # Volunteer verification flow
  handle_volunteer_lookup <- function(input) {
    switch(rv$current_question,
           "verify_name" = process_verify_volunteer_name(input),
           "verify_city" = process_verify_volunteer_city(input)
    )
  }
  
  process_verify_volunteer_name <- function(input) {
    if(!grepl(",", input)) {
      bot_message("Please enter your name in 'Last name, First name' format")
      return()
    }
    rv$lookup_data$name <- input
    bot_message("Which city are you registered in?")
    rv$current_question <- "verify_city"
  }
  
  # Replace the process_verify_volunteer_city function with this corrected version
  process_verify_volunteer_city <- function(input) {
    rv$lookup_data$city <- input
    volunteer_data <- read.csv("volunteers.csv", stringsAsFactors = FALSE) %>%
      filter(tolower(name) == tolower(rv$lookup_data$name),
             tolower(major_city) == tolower(rv$lookup_data$city))
    
    if(nrow(volunteer_data) == 0) {
      bot_message("No matching registration found. Would you like to register instead? (Yes/No)")
      rv$current_state <- "volunteer_registration_retry"
    } else {
      bot_message("Here are your registration details:")
      bot_message(paste(
        paste("Name:", volunteer_data$volunteer_name),
        paste("City:", volunteer_data$major_city),
        paste("State:", volunteer_data$state),
        paste("Availability:", volunteer_data$availability),
        sep = "\n"
      ))
      
      rv$current_volunteer_id <- volunteer_data$volunteer_id[1]
      
      if(file.exists("updated_matches.csv")) {
        matches <- read.csv("updated_matches.csv") %>%
          filter(volunteer_id == rv$current_volunteer_id)
        
        if(nrow(matches) > 0) {
          elderly <- read.csv("elderly.csv") %>%
            filter(elderly_id %in% matches$elderly_id)
          
          if(nrow(elderly) > 0) {
            bot_message("\n\nYour matched elderly person(s):")
            lapply(1:nrow(elderly), function(i) {
              bot_message(paste(
                paste("Name:", elderly$name[i]),
                paste("Age:", elderly$age[i]),
                paste("Location:", elderly$major_city[i], elderly$state[i]),
                paste("Preferred Times:", elderly$preferred_time_slots[i]),
                sep = "\n"
              ))
            })
          } else {
            bot_message("\nNo elderly found for your matches. Please check back later.")
          }
        } else {
          bot_message("\nNo current matches found. We'll notify you when we find a match.")
        }
      } else {
        bot_message("\nMatch system is currently unavailable. Please check back later.")
      }
      
      rv$current_state <- "volunteer_ready"
    }
  }
  
  
  
  handle_initial_input <- function(input) {
    if(tolower(input) %in% c("elderly", "elderly person", "elderly person seeking assistance")) {
      bot_message("Thank you for letting us know you're seeking assistance. Have you registered with us before? (Yes/No)")
      rv$current_state <- "elderly_registration_check"
    } else if(tolower(input) %in% c("volunteer", "v")) {
      bot_message("Thank you for volunteering! Have you registered with us before? (Yes/No)")
      rv$current_state <- "volunteer_registration_check"
      rv$user_type <- "volunteer"
    } else {
      # NEW: Fallback to FAQ if it's not recognized as 'elderly' or 'volunteer'
      bot_message(get_faq_answer(input))
      
      # Optionally, set the chatbot state to something like "faq_mode"
      # so next time user types, it goes directly to the fallback
      rv$current_state <- "faq_mode"
    }
  }
  
  handle_volunteer_registration_check <- function(input) {
    if(tolower(input) %in% c("yes", "y")) {
      bot_message("Let's verify your details. Please provide your full name (Last name, First name):")
      rv$current_state <- "volunteer_lookup"
      rv$current_question <- "verify_name"  # Add this line
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
  
  handle_elderly_registration <- function(input) {
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
    
    write.table(new_entry, "elderly.csv", append = TRUE, sep = ",", row.names = FALSE, col.names = FALSE)
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
