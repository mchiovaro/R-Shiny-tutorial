##### R Shiny Tutorial #####
#
# This demonstrates a simple way to collect data using a 
# Shiny app.
#
# Created by M. Chiovaro (@mchiovaro)
# Last updated: 01_10_2022

##### Set up #####

# load packages
library(shiny)

# set the output directory
output_location <- "data"

# Define what we want to save
input_data <- c("text")

# Define question
text <- textInput("text", "What is your age?")
reset <- function(session) {
  updateTextInput(session, "text", value = "")
}

##### Save the data #####
saveData <- function(input) {
  
  # transform into dataframe
  data <- data.frame(matrix(nrow=1,ncol=0))
  data$age <- input[[input_data]]
  
  # add the time of submission as a variable to be saved
  data$submit_time <- date()
  
  # Create a unique file name by order of participation
  num_responses <- length(list.files(output_location)) + 1
  fileName <- paste("./", output_location, "/", num_responses,".csv",sep="")
  
  # write data to file
  write.table(x = data,
              file=fileName,
              sep=",",
              col.names=TRUE,
              row.names=FALSE)

}

##### Set up the UI #####
ui <- fluidPage(
  h3("Survey"),
  p("You are being asked to participate in our study. 
    Please fill out the following questionnaire to the best of your ability."),
  fluidRow(
    column(width=6, text)
  ),
  actionButton("submit", "Submit")
)

##### Set up the server #####
server = function(input, output, session) {
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
    saveData(input)
    reset(session)
    
    # thank the user
    num_responses <- length(list.files(output_location))
    message <- paste0("Thank you for completing the survey! You were number ",
                       num_responses, " to respond.")
    showNotification(message, duration = 0, type = "message")
  })
  
}

# Run the app
shinyApp(ui, server)