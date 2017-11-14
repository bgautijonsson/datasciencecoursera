setwd("~/Data Science Specialization/Getting and Cleaning Data/Week 1")

download.file(url='https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv', destfile='2006housing.csv', method='curl')
dateDownloaded <- date()

data <- read.csv('2006housing.csv')
names(data)

# 1) How many properties are woth $1,000.000 or more?

answer <- data[data$VAL == 24, ]
answer <- answer$VAL
answer <- na.omit(answer)
length(answer)

# 2) Use the data you loaded from Question 1. Consider the variable FES 
# in the code book. Which of the "tidy data" principles does this variable 
# violate?

## A) Tidy data has one variable per column

# 3) Read rows 18-23 and columns 7-15 into R

fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx'
download.file(fileURL, destfile='Fdatagov.xlsx', method='curl')
library(xlsx)
dat <- read.xlsx('Fdatagov.xlsx', sheetIndex = 1, rowIndex = 18:23, colIndex = 7:15)
head(dat)
sum(dat$Zip*dat$Ext,na.rm=T)

# 4) XML scanning and exploring

fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml'
download.file(url = fileURL, destfile = 'baltRest.xml', method='curl')
library(XML)
doc <- xmlTreeParse('baltRest.xml', useInternalNodes = TRUE)
rootNode <- xmlRoot(doc)


names(rootNode)
rootNode[[1]][[1]]
zipcodes <- xpathSApply(rootNode, '//zipcode', xmlValue)
length(zipcodes[zipcodes == '21231'])


# 5) 

fileURL <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv'
download.file(fileURL, destfile = 'surveydata.csv', method='curl')

library(data.table)
DT <- fread('surveydata.csv')

## a)

mean(DT$pwgtp15,by=DT$SEX)
system.time(mean(DT$pwgtp15,by=DT$SEX))

## b)

DT[,mean(pwgtp15),by=SEX]
system.time(DT[,mean(pwgtp15),by=SEX])

## c)

mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
system.time(mean(DT[DT$SEX==1,]$pwgtp15)) + system.time(mean(DT[DT$SEX==2,]$pwgtp15))

## d)

sapply(split(DT$pwgtp15,DT$SEX),mean)
system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))

## e)



## f)

tapply(DT$pwgtp15,DT$SEX,mean)
system.time(tapply(DT$pwgtp15,DT$SEX,mean))
