library(ggplot2)
library(ggthemes)
library(gridExtra)
library(GGally)
library(googleVis)
library(caret)
library(rpart.plot)
library(randomForest)
library(knitr)
library(MASS)
library(memisc)
data(iris)

head(iris)
levels(iris$Species) <- c('Setosa', 'Versicolor', 'Virginica')

model <- train(x=iris[,-5], y=iris[,5], method='rpart')


ldamodel <- lda(formula = Species ~ ., data=iris, prior = c(1,1,1)/3, cv=TRUE)
predlda <- predict(ldamodel, iris)
predlda$class
predlda$x

proplda <- ldamodel$svd^2/sum(ldamodel$svd^2)
proplda

data <- data.frame(Species = iris$Species, lda = predlda$x)

g <- ggplot(data) + 
    geom_point(aes(lda.LD1, lda.LD2, colour=Species, shape = Species),
                               size=2.5, alpha=0.7)+
    labs(x='LD1(99.12%)',
         y = 'LD2(0.08%)') + theme_tufte(base_size = 14)
g

rpart.plot(x = model$finalModel, extra=4, tweak = 1.2, 
           box.palette = list('Reds', 'Blues', 'Greens'))

pred <- predict(model)
pred2 <- predict(ldamodel)
mean(pred == iris[,5])

confusionMatrix(pred2, iris$Species)
confusionMatrix(pred, iris$Species)

