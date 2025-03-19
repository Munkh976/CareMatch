library(shiny)
library(dplyr)
library(randomNames)
library(zipcodeR)

# Install if needed: install.packages(c("randomNames", "zipcode"))

# --- Generate Sample Data (Run this once) ---
set.seed(123)

# Generate 1000 elderly profiles
generate_elderly_profiles <- function(n) {
  data.frame(
    name = randomNames(n, which.names = "first.last"),
    age = sample(65:95, n, replace = TRUE),
    zipcode = sample(zip_code_db$zip, n, replace = TRUE),
    needs = replicate(n, paste(sample(c("Grocery help", "Transportation", "Companionship",
                                        "Meal prep", "Housekeeping", "Medical reminders",
                                        "Technology help", "Exercise companion"), 
                                      sample(1:3), replace = FALSE), collapse = ", ")),
    stringsAsFactors = FALSE
  ) %>% 
    left_join(zip_code_db, by = "zip") %>%
    select(name, age, city, state, needs)
}

# Generate 1000 volunteer profiles
generate_volunteer_profiles <- function(n) {
  data.frame(
    name = randomNames(n, which.names = "first.last"),
    city = sample(unique(zipcode_db$city), n, replace = TRUE),
    state = sample(state.abb, n, replace = TRUE),
    skills = replicate(n, paste(sample(c("Grocery help", "Transportation", "Companionship",
                                         "Meal prep", "Housekeeping", "Medical expertise",
                                         "Tech support", "Physical therapy"), 
                                       sample(1:4), replace = FALSE), collapse = ", ")),
    availability = replicate(n, paste(sample(c("Mon 9-12", "Tue 14-17", "Wed 10-13",
                                               "Thu 15-18", "Fri 11-14", "Sat 10-16"), 
                                             sample(2:3)), collapse = ", ")),
    stringsAsFactors = FALSE
  )
}

# Save to CSV
write.csv(generate_elderly_profiles(1000), "elderly_profiles.csv", row.names = FALSE)
write.csv(generate_volunteer_profiles(1000), "volunteer_profiles.csv", row.names = FALSE)

# --- Shiny App Code ---
elderly_profiles <- read.csv("elderly_profiles.csv", stringsAsFactors = FALSE)
volunteer_profiles <- read.csv("volunteer_profiles.csv", stringsAsFactors = FALSE)

all_needs <- unique(unlist(strsplit(elderly_profiles$needs, ", ")))
all_skills <- unique(unlist(strsplit(volunteer_profiles$skills, ", ")))

ui <- fluidPage(
  titlePanel("CareMatch - Interactive Elderly Care System"),
  tags$head(tags$style("
    .chatbox {height: 400px; overflow-y: auto; border: 1px solid #ccc; padding: 10px;}
    .user-message {color: #0066cc; margin-left: 20px;}
    .bot-message {color: #009933; margin-right: 20px;}
    .need-btn {margin: 3px; display: inline-block;}
  ")),
  
  sidebarLayout(
    sidebarPanel(
      textInput("user_input", "Type your response:"),
      actionButton("send", "Send"),
      uiOutput("dynamic_ui"),
      width = 3
    ),
    mainPanel(
      div(class = "chatbox", uiOutput("chat_history")),
      uiOutput("recommendations"),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    step = "welcome",
    user_data = list(),
    selected_needs = character(),
    messages = list()
  )
  
  # Conversation flow controller
  observeEvent(input$send, {
    current_step <- rv$step
    
    if(current_step == "welcome") {
      rv$messages <- append(rv$messages, list(list(type = "bot", text = "Welcome to CareMatch! What's your full name?")))
      rv$step <- "get_name"
    }
    else if(current_step == "get_name") {
      profile <- elderly_profiles %>% filter(tolower(name) == tolower(input$user_input))
      if(nrow(profile) > 0) {
        rv$user_data <- as.list(profile[1,])
        rv$selected_needs <- unlist(strsplit(rv$user_data$needs, ", "))
        rv$step <- "confirm_needs"
      } else {
        rv$user_data$name <- input$user_input
        rv$step <- "get_age"
      }
    }
    else if(current_step == "get_age") {
      rv$user_data$age <- input$user_input
      rv$step <- "get_location"
    }
    else if(current_step == "get_location") {
      rv$user_data$location <- input$user_input
      rv$step <- "select_needs"
    }
    else if(current_step %in% c("confirm_needs", "select_needs")) {
      rv$step <- "show_matches"
    }
    
    updateTextInput(session, "user_input", value = "")
  })
  
  # Dynamic UI for needs selection
  output$dynamic_ui <- renderUI({
    if(rv$step == "confirm_needs") {
      tagList(
        h4("Your typical needs:"),
        lapply(rv$selected_needs, function(need) {
          actionButton(need, need, class = "btn-primary need-btn")
        }),
        actionButton("change_needs", "Change Needs", class = "btn-warning")
      )
    }
    else if(rv$step == "select_needs") {
      tagList(
        h4("Select your needs:"),
        checkboxGroupInput("needs_selection", NULL, choices = all_needs)
      )
    }
  })
  
  # Chat history display
  output$chat_history <- renderUI({
    messages <- lapply(rv$messages, function(msg) {
      div(class = ifelse(msg$type == "user", "user-message", "bot-message"),
          strong(ifelse(msg$type == "user", "You:", "CareMatch:")),
          msg$text)
    })
    
    if(rv$step == "get_name") {
      messages <- append(messages, list(
        div(class = "bot-message", strong("CareMatch:"), "Please enter your full name")
      ))
    }
    else if(rv$step == "get_age") {
      messages <- append(messages, list(
        div(class = "bot-message", strong("CareMatch:"), "Nice to meet you! How old are you?")
      ))
    }
    else if(rv$step == "get_location") {
      messages <- append(messages, list(
        div(class = "bot-message", strong("CareMatch:"), "Please enter your city and state (e.g., 'New York, NY')")
      ))
    }
    
    tagList(messages)
  })
  
  # Volunteer matching logic
  output$recommendations <- renderUI({
    if(rv$step == "show_matches") {
      req(rv$user_data$city, rv$user_data$state)
      
      matches <- volunteer_profiles %>%
        filter(tolower(city) == tolower(rv$user_data$city),
               tolower(state) == tolower(rv$user_data$state)) %>%
        mutate(
          skills_list = strsplit(skills, ", "),
          match_score = sapply(skills_list, function(skills) {
            sum(rv$selected_needs %in% skills) / length(rv$selected_needs)
          })
        ) %>%
        filter(match_score > 0) %>%
        arrange(desc(match_score)) %>%
        head(5)
      
      tagList(
        h3("Top Matching Volunteers:"),
        if(nrow(matches) > 0) {
          lapply(1:nrow(matches), function(i) {
            vol <- matches[i,]
            div(
              h4(vol$name),
              p(strong("Skills:"), vol$skills),
              p(strong("Availability:"), vol$availability),
              p(strong("Match Score:"), round(vol$match_score * 100), "%"),
              hr()
            )
          })
        } else {
          p("No matches found. Expanding search radius...")
        }
      )
    }
  })
}

shinyApp(ui, server)
