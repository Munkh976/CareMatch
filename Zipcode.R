library(shiny)
library(zipcodeR)  # Load the package explicitly

ui <- fluidPage(
  titlePanel("US ZIP Code to Coordinates Converter"),
  sidebarLayout(
    sidebarPanel(
      textInput("zip_input", "Enter 5-Digit US ZIP Code:", 
                placeholder = "e.g., 43607"),
      actionButton("submit", "Get Coordinates")
    ),
    mainPanel(
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$submit, {
    req(input$zip_input)
    
    clean_zip <- gsub("[^0-9]", "", input$zip_input)
    if(nchar(clean_zip) != 5) {
      output$result <- renderText("Invalid format - must be 5 digits")
      return()
    }
    
    # Use zipcodeR's function with error handling
    tryCatch({
      result <- zipcodeR::search_zipcode(clean_zip)  # Explicit package call
      
      if(nrow(result) > 0) {
        output$result <- renderText({
          paste(
            "ZIP Code:", result$zipcode,
            "\nCity:", result$post_office_city,
            "\nState:", result$state,
            "\nLatitude:", result$lat,
            "\nLongitude:", result$lng
          )
        })
      } else {
        output$result <- renderText("ZIP code not found in US database")
      }
    }, error = function(e) {
      output$result <- renderText("Error retrieving data")
    })
  })
}

shinyApp(ui = ui, server = server)