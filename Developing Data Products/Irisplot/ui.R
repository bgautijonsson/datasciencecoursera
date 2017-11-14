
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Iris Flowers Data"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4('The Iris Dataset'),
            h6('This dataset was introduced by Ronald Fisher in 1936.The data set 
               consists of 50 samples from each of three species of the Iris flower, 
               Setosa, Versicolor and Virginica.'),
            h6(''),
            h6('Four features were measured from each sample: the length and the width 
               of the sepals and petals, in centimetres. '),
            h6(''),
            h6('Based on the combination of these four features, Fisher developed a 
               linear discriminant model to distinguish the species from each other.'),
            h4('Choose which flower species to plot:'),
            checkboxInput('setosaId', 'Setosa', value=TRUE),
            checkboxInput('versicolorId', 'Versicolor', value=TRUE),
            checkboxInput('virginicaId', 'Virginica', value=TRUE),
            selectInput('predictorId', 'Choose a Predictor', 
                        list('Sepal Length' = 'Sepal Length',
                             'Sepal Width' = 'Sepal Width',
                             'Petal Length' = 'Petal Length',
                             'Petal Width' = 'Petal Width'))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel('Correlation', br(), plotOutput('plot1')),
                        tabPanel('Density', br(), plotOutput('plot2')),
                        tabPanel('Prediction Model',h3(textOutput('header1')), 
                                 textOutput('text1'),
                                 plotOutput('plot3'))
            )
        )
    )
))
