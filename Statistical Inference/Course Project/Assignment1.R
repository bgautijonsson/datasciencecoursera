library(ggplot2)
library(ggthemes)

set.seed(101)
exp <- NULL
for (i in 1:1000) exp <- c(exp, mean(rexp(40, 0.2)))

g <- ggplot(data.frame(x = exp), aes(x=x)) + theme_tufte()

g + geom_histogram(aes(y = ..density..), fill = 'royalblue2', color='black',
                   alpha = 0.7, binwidth = .1) + 
    stat_function(fun = 'dnorm', args=list(mean=1/0.2, sd = (0.2 * sqrt(40))^-2)) +
    xlab('Mean') + ylab('Density') + ggtitle('Density and Histogram Plot')
    
