#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(caret)
library(randomForest) 
load("trained_model.RData")  
shinyServer(function(input, output) {
  
  observeEvent(input$predict, {
    new_data <- data.frame(
      ASDGSCM = as.numeric(input$ASDGSCM),
      ASDGHRL = as.numeric(input$ASDGHRL),
      ASDHEDUP = as.numeric(input$ASDHEDUP),
      ASBH11 = as.numeric(input$ASBH11),
      ACDGTIHY = as.numeric(input$ACDGTIHY)
    )
    
   
    new_data <- new_data[, top_vars_rfe]
    
    # Make prediction
    prediction <- predict(model_rf, new_data)
    
    # Show result
    output$prediction_output <- renderText({
      paste("Predicted Math Performance:", as.character(prediction))
    })
  })
  
})