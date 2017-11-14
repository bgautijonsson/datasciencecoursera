
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Predict Horsepower from MPG"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput('sliderMPG', 'What is the MPG of the car?', 10, 35, value=20),
      checkboxInput('showModel1', 'Show/Hide Model 1', value=TRUE),
      checkboxInput('showModel2', 'Show/Hide Model 2', value=TRUE),
      submitButton('Submit')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot1"),
      h3('Predicted Horsepower from Model 1:'),
      textOutput('pred1'),
      h3('Predicted Horsepower from Model 2:'),
      textOutput('pred2')
    )
  )
))
