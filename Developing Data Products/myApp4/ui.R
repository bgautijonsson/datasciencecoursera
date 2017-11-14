library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Visualize Many Models"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3('Slope'),
      textOutput('slopeOut'),
      h3('Intercept'),
      textOutput('intOut')
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput('plot1', brush=brushOpts(
          id = 'brush1'
      ))
    )
  )
))
