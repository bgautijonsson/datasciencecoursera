# 1)
library(tidyr)
library(readr)

url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv'
download.file(url, destfile = 'ACS.csv')

df <- read_csv('ACS.csv')
names(df)

split_list <- sapply(names(df), function(data) strsplit(data, 'wgtp'))
split_list[123]


# 2)

url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv'
download.file(url, destfile = 'GDP.csv')
df[2:3,]
df <- read_csv('GDP.csv')
df <- df[, c(1, 2, 4, 5)]
names(df) <- c('countrycode', 'rank', 'economy', 'gdp')
df <- df[df$rank %in% 1:190, ]

df$gdp <- gsub(',', '', df$gdp)
df$gdp <- as.numeric(df$gdp)

mean(df$gdp)


# 3)

df[grep('^United', df$economy),]


# 4)



url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv'
download.file(url, destfile = 'edu.csv')

gdp <- df
edu <- read.csv('edu.csv')

gdpedu <- merge(gdp, edu, by.x='countrycode', by.y = 'CountryCode')

gdpedu$Special.Notes <- as.character(gdpedu$Special.Notes)

fiscalyear <- gdpedu[grep('[Ff]iscal year end', gdpedu$Special.Notes), 'Special.Notes']
fiscalyear

# 5)

library(quantmod)

amzn <- getSymbols("AMZN",auto.assign=FALSE)
sampleTimes <- index(amzn)

head(sampleTimes)
length(grep('2012', sampleTimes))

library(lubridate)

sampleDates <- ymd(sampleTimes)
head(sampleDates)

