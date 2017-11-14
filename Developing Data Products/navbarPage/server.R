library(shiny)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(caret)
library(rpart)
library(rpart.plot)
library(e1071)
library(MASS)
library(memisc)

shinyServer(function(input, output) {
        options(shiny.sanitize.errors = FALSE)
        data(iris)
        levels(iris$Species) <- c('Setosa','Versicolor','Virginica')
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
                        
                        if (!input$setosaId) { iris <- iris[iris$Species!='Setosa',]}
                        if (!input$versicolorId) { iris <- iris[iris$Species!='Versicolor',]}
                        if (!input$virginicaId) { iris <- iris[iris$Species!='Virginica',]}
                        
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
                if (!input$setosaId) { iris <- iris[iris$Species!='Setosa',]}
                if (!input$versicolorId) { iris <- iris[iris$Species!='Versicolor',]}
                if (!input$virginicaId) { iris <- iris[iris$Species!='Virginica',]}
                
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
        
        output$text1 <- renderText({
                'Second, a model was fit using decision trees. The training set accuracy was found to
                be 96%.. The decision parameters can be seen below and the
                percentage of observasions in each category are written inside the boxes. The
                model only used the features Petal Width and Petal Length for discrimination.
                If we look at the density plots of those variables we can see that most of
                the difference between species is found there.'
        })
        
        
        model <- train(x=iris[,-5], y=iris[,5], method='rpart')
        output$plot3 <- renderPlot({
                
                rpart.plot(x = model$finalModel, extra=4, tweak = 1.2, 
                           box.palette = list('Reds', 'Blues', 'Greens'))
                
        }, height = 550, width = 800)
        
        pred <- reactive({
                lengthInput <- input$sliderPetalLength
                widthInput <- input$sliderPetalWidth
                pred <- predict(model, newdata = 
                                        data.frame(Petal.Length=lengthInput,
                                                   Petal.Width=widthInput,
                                                   Sepal.Length=mean(iris$Sepal.Length),
                                                   Sepal.Width=mean(iris$Sepal.Width)))
                species <- c('Setosa', 'Versicolor', 'Virginica')
                return(species[pred])
        })
        
        output$pred <- renderText({
                pred()
        })
        output$text2 <- renderText({
                "First a fit was found via Linear Discriminant Analysis (LDA) also
                known as a generalization of Fisher's Linear Discriminant. LDA tries
                to find a linear combination of features that characterizes or 
                separates two or more classes of objects or events. The resulting
                combination is used for dimensionality reduction and then 
                classification. In the parantheses by the axes you can see the
                percentage of variance in the data explained by each discriminant. The 
                training set accuracy was 98% with this approach."
        })
        ldamodel <- lda(formula = Species ~ ., data=iris, prior = c(1,1,1)/3, cv=TRUE)
        
        output$plot4 <- renderPlot({
                predlda <- predict(ldamodel, iris)
                proplda <- ldamodel$svd^2/sum(ldamodel$svd^2)
                data <- data.frame(Species = iris$Species, lda = predlda$x)
                
                lengthInput1 <- input$sliderPetalLength
                widthInput1 <- input$sliderPetalWidth
                lengthInput2 <- input$sliderSepalLength
                widthInput2 <- input$sliderSepalWidth
                datapred <- data.frame(Petal.Length = lengthInput1,
                                   Petal.Width = widthInput1,
                                   Sepal.Length = lengthInput2,
                                   Sepal.Width = lengthInput2)
                pred <- data.frame(lda = predict(ldamodel, datapred)$x)
                g <- ggplot(data) + 
                        geom_point(aes(lda.LD1, lda.LD2, colour=Species, shape = Species),
                                   size=2.5, alpha=0.7)+
                        labs(x='Linear Discriminant 1 (99.12%)',
                             y = 'Linear Discriminant 2 (0.08%)') + 
                        theme_tufte(base_size = 14) + 
                        geom_point(data = pred, aes(lda.LD1, lda.LD2), colour='goldenrod2',
                                   size=6)
                g
        })
        
        output$pred2 <- renderText({
                lengthInput1 <- input$sliderPetalLength
                widthInput1 <- input$sliderPetalWidth
                lengthInput2 <- input$sliderSepalLength
                widthInput2 <- input$sliderSepalWidth
                data <- data.frame(Petal.Length = lengthInput1,
                                   Petal.Width = widthInput1,
                                   Sepal.Length = lengthInput2,
                                   Sepal.Width = lengthInput2)
                
                pred <- predict(ldamodel, data)
                species <- c('Setosa', 'Versicolor', 'Virginica')
                return(species[pred$class])
        })
        onSessionEnded(stopApp)
})
