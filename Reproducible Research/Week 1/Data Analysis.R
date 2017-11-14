library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(extrafont)
library(mice)
library(ggthemes)

# Reading and preprocessing
data <- read.csv('activity.csv')
head(data)
str(data)
unique(data$interval)
data$date <- as.Date(data$date, format='%Y-%m-%d')
# Plotting steps per day
g <- ggplot(data, aes(x=date, y=steps))
g + geom_col(na.rm = TRUE) + xlab('Date') + ylab('Steps') + 
    ggtitle('Plot of Step Count for Each Day')
# Finding the median of num steps

sum <- data %>%
    group_by(date) %>%
    summarise(sum = sum(steps)) %>%
    arrange(desc(sum))
head(sum)

gghist <- ggplot(data = sum, aes(sum))

gghist + geom_histogram(na.rm = TRUE, binwidth = 2000) + theme_tufte() + 
    xlab('Steps') + ylab('Days') + ggtitle('Total Step Counts')


median(data$steps, na.rm=TRUE)
sum(na.omit(data$steps) == 0)


# Convert interval to time variable
intervaltime <- str_pad(unique(data$interval), 4, pad='0')
intervaltime <- format(strptime(intervaltime, format='%H%M'), format='%H:%M')
intervaltime <- unique(intervaltime)
intervaltime[seq(1, 288, by=288/24)]

# Plotting the number of steps for each 5 min interval in a day
g2 <- ggplot(data, aes(x=interval, y=steps))
g2 + stat_summary(fun.y=mean, geom='line', na.rm=TRUE) + xlab('Time of Day') +
    ylab('Mean Number of Steps') + 
    ggtitle('Mean Number of Steps For Different Times of Day') +
    theme_tufte(base_size = 12) +
    scale_x_continuous(breaks=seq(1, 2355, length.out = 12), 
                       labels=intervaltime[seq(1, 288, by=288/12)])

# Find the highest mean num steps per interval
means <- data %>%
    group_by(interval) %>%
    summarise(avgsteps = mean(steps, na.rm=TRUE)) %>%
    arrange(desc(avgsteps))


head(means)
# Imputing missing values

data[is.na(data),]

impute <- data %>% 
    group_by(interval) %>%
    mutate_at(., 1,funs(replace(., which(is.na(.)), mean(.,na.rm=TRUE))))

mean(group_by(data, interval)$steps, na.rm=TRUE)

g3 <- ggplot(impute, aes(x=date, y=steps))
g3 + geom_col() + xlab('Date') + ylab('Total Steps') +
    ggtitle('Total Steps Datewise')

# Weekdays vs weekends


impute$weekday <- weekdays(impute$date)
impute[impute$weekday %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday',
                             'Friday'),4] <- 'Weekday'
impute[impute$weekday %in% c('Saturday', 'Sunday'),4] <- 'Weekend'

g2 <- ggplot(impute, aes(x=interval, y=steps))
g2 + stat_summary(fun.y=mean, geom='line', na.rm=TRUE) + xlab('Time of Day') +
    ylab('Mean Number of Steps') + 
    ggtitle('Mean Number of Steps For Different Times of Day') +
    theme_tufte(base_size = 12) +
    scale_x_continuous(breaks=seq(1, 2355, length.out = 12), 
                       labels=intervaltime[seq(1, 288, by=288/12)]) +
    facet_grid(. ~ weekday)
