library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(tibble)
library(gridExtra)
library(stringr)
# Reading the data

temp <- tempfile()
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2', temp)
data <- read.csv(temp)


data <- read.csv('repdata%2Fdata%2FStormData.csv')

# Exploratory data analysis

head(data[, c(8, 23, 24, 25, 27)], 20)
names(data)
str(data)

smalldata <- data[, c(8, 23, 24, 25, 26, 27, 28)]
names(smalldata)


# Let's summarise by finding the means of population and property damage by 
# event type
means <- smalldata %>%
    group_by(EVTYPE) %>%
    summarise(meanfat=mean(FATALITIES), meaninj=mean(INJURIES), 
              meanpropdmg=mean(PROPDMG), meancropdmg=mean(CROPDMG))


names <- means[c(grep("TROPICAL STORM ",means$EVTYPE), grep("HURRICANE ", means$EVTYPE)), 1]
means[means$EVTYPE %in% names$EVTYPE,]


# Now find the top 50 event types for fatalities and injuries
meanfatal <- arrange(means, desc(meanfat))[1:100,c(1,2)]
meaninj <- arrange(means, desc(meaninj))[1:100,c(1,3)]
# Look at the top 10 for each category

cbind(head(meaninj, 10), head(meanfatal, 10))

# Find which event types are in both lists
meanpop <- arrange(merge(meanfatal, meaninj, by ='EVTYPE'), desc(meanfat))
gpop <- ggplot(meanpop[1:10,]) + xlab('Event Type') + theme_tufte()
gfat <- gpop + geom_col(aes(x = EVTYPE, y = meanfat)) + ylab('Mean Fatality Count') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ginj <- gpop + geom_col(aes(x = EVTYPE, y = meaninj)) + ylab('Mean Injury Count') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
grid.arrange(gfat, ginj)

# Do the same for property damage
meanprop <- arrange(means, desc(meanpropdmg))[1:100,c(1,4)]
meancrop <- arrange(means, desc(meancropdmg))[1:100,c(1,5)]

# Look at the top 10 for each category
cbind(head(meancrop, 10), head(meanprop, 10))

# This code shows the top 10 for property with crop damage added
meaneco <- arrange(merge(meanprop, meancrop, by = 'EVTYPE'), desc(meanpropdmg))
geco <- ggplot(meaneco[1:10,]) + xlab('Event Type') + theme_tufte(base_size = 10)
gprop <- geco + geom_col(aes(x=EVTYPE, y=meanpropdmg)) + 
    ylab('Property damage') + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
gcrop <- geco + geom_col(aes(x=EVTYPE, y=meancropdmg)) + 
    ylab('Crop Damage') + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
grid.arrange(gprop, gcrop)

# Make a plot of events that scored high on all measures
meanall <- merge(meaneco, meanpop, by = 'EVTYPE')
gall <- ggplot(meanall[1:5,]) + xlab('Event Type') + theme_tufte(base_size = 10)
gpropall <- gall + geom_col(aes(x=EVTYPE, y=meanpropdmg)) + 
    ylab('Property damage') + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
gcropall <- gall + geom_col(aes(x=EVTYPE, y=meancropdmg)) + 
    ylab('Crop Damage') + 
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
gfatall <- gall + geom_col(aes(x = EVTYPE, y = meanfat)) + ylab('Mean Fatality Count') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
ginjall <- gall + geom_col(aes(x = EVTYPE, y = meaninj)) + ylab('Mean Injury Count') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
grid.arrange(gpropall, gcropall, gfatall, ginjall, ncol = 2)




# Now to explore the totals instead of means

sums <- smalldata %>%
    group_by(EVTYPE) %>%
    summarise(sumfat=sum(FATALITIES), suminj=sum(INJURIES), 
              sumpropdmg=sum(PROPDMG), sumcropdmg=sum(CROPDMG))
sumfat <- arrange(sums, desc(sumfat))[1:100, c(1,2)]
suminj <- arrange(sums, desc(suminj))[1:100, c(1,3)]
sumprop <- arrange(sums, desc(sumpropdmg))[1:100, c(1,4)]
sumcrop <- arrange(sums, desc(sumpropdmg))[1:100, c(1,5)]

gsumfat <- ggplot(sumfat[1:5,], aes(x=EVTYPE, y=sumfat)) + geom_col() + 
    theme_tufte() + xlab('Event Type') + ylab('Total Fatalities') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
gsuminj <- ggplot(suminj[1:5,], aes(x=EVTYPE, y=suminj)) + geom_col() + 
    theme_tufte() + xlab('Event Type') + ylab('Total Injuries') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
gsumprop <- ggplot(sumprop[1:5,], aes(x=EVTYPE, y=sumpropdmg)) + geom_col() + 
    theme_tufte() + xlab('Event Type') + ylab('Total Property Damage') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
gsumcrop <- ggplot(sumcrop[1:5,], aes(x=EVTYPE, y=sumcropdmg)) + geom_col() + 
    theme_tufte() + xlab('Event Type') + ylab('Total Crop Damage') +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10))
grid.arrange(gsumfat, gsuminj, gsumprop, gsumcrop, ncol=2)

