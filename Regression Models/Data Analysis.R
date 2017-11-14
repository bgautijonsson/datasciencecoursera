library(ggplot2)
library(ggthemes)
library(GGally)
library(gridExtra)

# Reading and Exploring Data
data("mtcars")
str(mtcars)
head(mtcars)
mtcars$am <- as.factor(mtcars$am)
levels(mtcars$am) <- c("Auto", "Manual")
mtcars$cyl <- as.factor(mtcars$cyl)
mtcars$vs <- as.factor(mtcars$vs)
mtcars$gear <- as.factor(mtcars$gear)
mtcars$carb <- as.factor(mtcars$carb)


# Plots
data <- mtcars[, -c(4, 7, 8, 10, 11)]

ggpairs(data, aes(fill=am), lower=list(combo=wrap("facethist", binwidth=0.8)))

# Boxplot of relationship between mpg ~ am + cyl
g1 <- ggplot(mtcars) + geom_boxplot(aes(x=am, y=mpg, fill=cyl)) + theme_tufte() +
    facet_grid(facets = .~cyl) + xlab('Transmission Type') + 
    labs(fill='Cylinders') + ylab('Miles Per Gallon')
g1
# Points plot of relationship between mpg ~ am + wt
g2 <- ggplot(mtcars) + geom_point(aes(x=wt, y=mpg, col=am)) + theme_tufte() +
    geom_smooth(aes(x=wt, y=mpg), method='lm') +xlab('Weight (in 1000lbs)') + 
    ylab('Miles Per gallon') + labs(col='Transmission Type')
g2
grid.arrange(g1, g2)
# First look at linear regressors

fitall <- lm(mpg ~ . , mtcars)
summary(fitall)
anova(fitall)

data <- mtcars[, c(1,2,6,9)]
names(data) <- c('Miles Per Gallon', 'Cylinders','Weight', 'Transmission')
pairs <- ggpairs(data, aes(fill=Transmission, col=Transmission), 
        lower=list(combo=wrap("facethist", binwidth=0.8)),
        title='Variable Analysis by Transmission Type',
        legend = c(3,3)) + 
    theme_tufte(base_size = 13) + theme(legend.text=element_text(size=8), 
                                        legend.title = element_text(size=10))
pairs


fitall2 <- lm(mpg ~ wt + am -1, mtcars)
summary(fitall2)
anova(fitall2)
plot(fitall2)
anova(lm(mpg ~ wt, mtcars), lm(mpg ~ wt + cyl -1 , mtcars))



