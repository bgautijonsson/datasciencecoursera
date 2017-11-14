library(datasets)
library(ggplot2)
library(ggthemes)

data(ToothGrowth)
str(ToothGrowth)
summary(ToothGrowth)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
levels(ToothGrowth$supp) <- c('Orange Juice', 'Ascorbic Acid')

table(ToothGrowth$supp, ToothGrowth$dose)

g <- ggplot(ToothGrowth, aes(y=len, x=dose)) + theme_tufte()
g + geom_boxplot(aes(fill = supp)) + facet_grid(.~supp) + xlab('Dose (mg/day)') +
    ylab('Cell Length') + labs(fill = 'Method') + 
    scale_color_manual(labels = c('Orange Juice', 'Ascorbic Acid')) + 
    ggtitle('Tooth Growth in Mice by Dose Type and Magnitude')


for (i in c(0.5, 1, 2)) {
    print(t.test(len ~ supp, data = ToothGrowth[ToothGrowth$dose==i,]))
}

        

supptest <- t.test(len ~ supp, data = ToothGrowth)
supptest   

for (i in c(2, 0.5, 1)) {
    print(t.test(len ~ dose, data = ToothGrowth[ToothGrowth$dose!=i,]))
}
