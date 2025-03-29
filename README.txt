# Student Math Performance Prediction App

This is a web-based Shiny application that predicts a student's math performance level ? **High**, **Medium**, or **Low** ? based on five key input variables related to the student's learning environment and background.

The prediction is powered by a machine learning classification model (Random Forest) trained on educational data.

Firstly, I used the TIMSS data 2023 in Poland for mathematics to do feature selection and prediction. Then I find the best algorithms for prediction and put it into the shiny application. 

---

##  Features

- Predicts math performance in **3 levels**: High / Medium / Low
- Interactive input interface with descriptive options
- Built with R and Shiny
- Easily deployable on [shinyapps.io](https://shinyapps.io)

---

## Variables Used in the Model

| Variable      | Description                              | Value Meaning |
|---------------|------------------------------------------|---------------|
| `ASDGSCM`     | Student Confidence in Mathematics         | 1 = Very Confident, 2 = Somewhat Confident, 3 = Not Confident |
| `ASDGHRL`     | Home Resources for Learning               | 1 = Many, 2 = Some, 3 = Few |
| `ASDHEDUP`    | Parents' Highest Education Level          | 1 = University+, 2 = Post-secondary (non-uni), ..., 6 = Not Applicable |
| `ASBH11`      | Number of Children's Books at Home        | 1 = 0?10, ..., 5 = 100+ |
| `ACDGTIHY`    | Total Instructional Hours per Year        | Numeric
 (e.g., 800?1500) |

## About the files 
1 R file called basics is the machine learning part for prediction and classification including the preprocessing of the data 
2 floder called student performance is the shiny application code 
3 Excel file Codebook contains all of the description for all of the variables and the colored tab in the file shows the selected variables that are used in the project
        
