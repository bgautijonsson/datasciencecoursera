# 2-3)

library(sqldf)

download.file('https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv', destfile = 'ACSurvey.csv', method='curl')

acs <- read.csv('ACSurvey.csv')

sqldf("select distinct AGEP from acs")


# 4)
library(XML)

url <- 'http://biostat.jhsph.edu/~jleek/contact.html'

htmlcode <- htmlTreeParse(url, useInternalNodes = TRUE)
htmlcode

xpathSApply(htmlcode, '//title', xmlValue)

library(httr)

html2 <- GET(url)
content <- content(html2, as='text')
parsedHtml <- htmlParse(content, asText = TRUE)
##########

download.file(url, destfile = 'biostat')
text <- readLines('biostat')
sapply(text[c(10, 20, 30, 100)], nchar)



## 5)
library(xlsx)
url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fwksst8110.for'

download.file(url, destfile = 'q5')
file1 <- readLines('q5')

file2 <- read.fwf('q5', widths = c(15,4,4,9,4,9,4,9,4), skip=4)
sum(file2[, 4])


