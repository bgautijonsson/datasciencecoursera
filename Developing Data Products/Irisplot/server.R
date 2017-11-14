library(shiny)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)

shinyServer(function(input, output) {
    options(shiny.sanitize.errors = FALSE)
    data(iris)
    levels(iris$Species) <- c('Setosa', 'Versicolor', 'Virginica')
    
    output$plot1 <- renderPlot({
        
        if (!input$setosaId) { iris <- iris[iris$Species!='Setosa',]}
        if (!input$versicolorId) { iris <- iris[iris$Species!='Versicolor',]}
        if (!input$virginicaId) { iris <- iris[iris$Species!='Virginica',]}
        g <- ggplot(data = iris, 
                    aes(col=Species)) + 
            theme_tufte(base_size = 14)
        if (input$predictorId == 'Sepal Length') {
            g1 <- g + geom_point(aes(x=Sepal.Length, y=Petal.Width)) + 
                geom_smooth(aes(x=Sepal.Length, y=Petal.Width), method='loess') +
                xlab('') + ylab('Petal Width')
            g2 <- g + geom_point(aes(x=Sepal.Length, y=Sepal.Width)) +
                geom_smooth(aes(x=Sepal.Length, y=Sepal.Width), method='loess') +
                xlab('') + ylab('Sepal Width')
            g3 <- g + geom_point(aes(x=Sepal.Length, y=Petal.Length)) +
                geom_smooth(aes(x=Sepal.Length, y=Petal.Length), method='loess') +
                xlab('Sepal Length') + ylab('Petal Length')
        }
        if (input$predictorId == 'Sepal Width') {
            g1 <- g + geom_point(aes(x=Sepal.Width, y=Petal.Width)) + 
                geom_smooth(aes(x=Sepal.Width, y=Petal.Width), method='loess') +
                xlab('') + ylab('Petal Width')
            g2 <- g + geom_point(aes(x=Sepal.Width, y=Sepal.Length)) +
                geom_smooth(aes(x=Sepal.Width, y=Sepal.Length), method='loess') +
                xlab('') + ylab('Sepal Length')
            g3 <- g + geom_point(aes(x=Sepal.Width, y=Petal.Length)) +
                geom_smooth(aes(x=Sepal.Width, y=Petal.Length), method='loess') +
                xlab('Sepal Width') + ylab('Petal Length')
        }
        
        if (input$predictorId == 'Petal Length') {
            g1 <- g + geom_point(aes(x=Petal.Length, y=Petal.Width)) + 
                geom_smooth(aes(x=Petal.Length, y=Petal.Width), method='loess') +
                xlab('') + ylab('Petal Width')
            g2 <- g + geom_point(aes(x=Petal.Length, y=Sepal.Width)) +
                geom_smooth(aes(x=Petal.Length, y=Sepal.Width), method='loess') +
                xlab('') + ylab('Sepal Width')
            g3 <- g + geom_point(aes(x=Petal.Length, y=Sepal.Length)) +
                geom_smooth(aes(x=Petal.Length, y=Sepal.Length), method='loess') +
                xlab('Petal Length') + ylab('Sepal Length')
        }
        
        if (input$predictorId == 'Petal Width') {
            
            if (!input$setosaId) { iris <- iris[iris$Species!='setosa',]}
            if (!input$versicolorId) { iris <- iris[iris$Species!='versicolor',]}
            if (!input$virginicaId) { iris <- iris[iris$Species!='virginica',]}
            
            g1 <- g + geom_point(aes(x=Petal.Width, y=Petal.Length)) + 
                geom_smooth(aes(x=Petal.Width, y=Petal.Length), method='loess') +
                xlab('') + ylab('Petal Length') + ylim(c(0,7))
            g2 <- g + geom_point(aes(x=Petal.Width, y=Sepal.Width)) +
                geom_smooth(aes(x=Petal.Width, y=Sepal.Width), method='loess') +
                xlab('') + ylab('Sepal Width') + ylim(c(0,6))
            g3 <- g + geom_point(aes(x=Petal.Width, y=Sepal.Length)) +
                geom_smooth(aes(x=Petal.Width, y=Sepal.Length), method='loess') +
                xlab('Petal Width') + ylab('Sepal Length') + ylim(c(2,7))
        }
        
        grid.arrange(g1, g2, g3)
        
    }, height = 550, width = 800)
    
    output$plot2 <- renderPlot({
        if (!input$setosaId) { iris <- iris[iris$Species!='setosa',]}
        if (!input$versicolorId) { iris <- iris[iris$Species!='versicolor',]}
        if (!input$virginicaId) { iris <- iris[iris$Species!='virginica',]}
        
        g <- ggplot(data = iris, aes(fill=Species)) + 
            theme_tufte(base_size = 16)
        
        if (input$predictorId == 'Petal Width'){
            g + geom_density(aes(x=Petal.Width), alpha=0.4) + 
                ylab('Density') + xlab('Petal Width')
        }
        
        else if (input$predictorId == 'Petal Length') {
            g + geom_density(aes(x=Petal.Length), alpha=0.4) + 
                ylab('Density') + xlab('Petal Length')
        }
        
        else if (input$predictorId == 'Sepal Width') {
            g + geom_density(aes(x=Sepal.Width), alpha=0.4) + 
                ylab('Density') + xlab('Sepal Width')
        }
        else if (input$predictorId == 'Sepal Length') {
            g + geom_density(aes(x=Sepal.Length), alpha=0.5) + 
                ylab('Density') + xlab('Sepal Length')
        }
    }, height = 550, width = 800)
    
    output$header1 <- renderText({
        'Simple Decision Tree'
    })
    output$text1 <- renderText({
        'A model was fit using decision trees. The training set accuracy was found to
        be 96%. No test set was used. The decision parameters can be seen below. We can
        see from the density plots why the measures of the Petal were used as predictors.'
    })
    
    output$plot3 <- renderPlot({
        model <- train(x=iris[,-5], y=iris[,5], method='rpart')
        
        rpart.plot(x = model$finalModel, extra=4, tweak = 1.2, 
                   box.palette = list('Reds', 'Blues', 'Greens'))
        
    }, height = 550, width = 800)
    onSessionEnded(stopApp)
})
