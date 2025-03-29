#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/

library(shiny)

# ASDGSCM	Students Confident in Mathematics/IDX	1: Very Confident in Mathematics; 2: Somewhat Confident in Mathematics; 3: Not Confident in Mathematics
# ASDGHRL	Home Resources for Learning/IDX	1: Many Resources; 2: Some Resources; 3: Few Resources
# ASDHEDUP	Parents' Highest Education Level	1: University or Higher; 2: Post-Secondary Education but not University; 3: Upper Secondary; 4: Lower Secondary; 5: Some Primary or Lower Secondary or Did not go to School; 6: Not Applicable
# ASBH11	Amount of books for children at home	1: 0–10; 2: 11–25; 3: 26–50; 4: 51–100; 5: More than 100
# ACDGTIHY	Total Instructional Hours per Year

# actual top 5 predictors
predictors <- c("ASDGSCM", "ASDGHRL", "ASDHEDUP", "ASBH11", "ACDGTIHY")

shinyUI(fluidPage(
  titlePanel("📊 Student Math Performance Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      
      h4("📘 Variable Descriptions"),
      
      helpText(HTML("<b>ASDGSCM</b>: Student Confidence in Mathematics<br>
                    1 = Very Confident<br>
                    2 = Somewhat Confident<br>
                    3 = Not Confident")),
      selectInput("ASDGSCM", "Confidence in Mathematics:",
                  choices = c("1 = Very Confident" = 1,
                              "2 = Somewhat Confident" = 2,
                              "3 = Not Confident" = 3)),
      
      helpText(HTML("<b>ASDGHRL</b>: Home Resources for Learning<br>
                    1 = Many Resources<br>
                    2 = Some Resources<br>
                    3 = Few Resources")),
      selectInput("ASDGHRL", "Home Resources for Learning:",
                  choices = c("1 = Many Resources" = 1,
                              "2 = Some Resources" = 2,
                              "3 = Few Resources" = 3)),
      
      helpText(HTML("<b>ASDHEDUP</b>: Parents' Highest Education Level<br>
                    1 = University or Higher<br>
                    2 = Post-secondary (Not University)<br>
                    3 = Upper Secondary<br>
                    4 = Lower Secondary<br>
                    5 = Some Primary / No Schooling<br>
                    6 = Not Applicable")),
      selectInput("ASDHEDUP", "Parents' Highest Education Level:",
                  choices = c("1 = University or Higher" = 1,
                              "2 = Post-secondary (Not University)" = 2,
                              "3 = Upper Secondary" = 3,
                              "4 = Lower Secondary" = 4,
                              "5 = Primary / No Schooling" = 5,
                              "6 = Not Applicable" = 6)),
      
      helpText(HTML("<b>ASBH11</b>: Number of Children's Books at Home<br>
                    1 = 0–10 books<br>
                    2 = 11–25 books<br>
                    3 = 26–50 books<br>
                    4 = 51–100 books<br>
                    5 = More than 100 books")),
      selectInput("ASBH11", "Children's Books at Home:",
                  choices = c("1 = 0–10 books" = 1,
                              "2 = 11–25 books" = 2,
                              "3 = 26–50 books" = 3,
                              "4 = 51–100 books" = 4,
                              "5 = More than 100 books" = 5)),
      
      helpText(HTML("<b>ACDGTIHY</b>: Total Instructional Hours per Year<br>
                    A positive number, typically between 200 and 1500 hours")),
      numericInput("ACDGTIHY", "Instructional Hours per Year:",
                   value = 800, min = 200, max = 1500, step = 10),
      
      br(),
      actionButton("predict", "🔮 Predict Performance")
    ),
    
    mainPanel(
      h3("Predicted Math Performance Category:"),
      verbatimTextOutput("prediction_output")
    )
  )
))