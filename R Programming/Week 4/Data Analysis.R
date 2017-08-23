data <- read.csv('outcome-of-care-measures.csv', na.strings =
                   'Not Available', stringsAsFactors = FALSE)
head(data)
str(data)
names(data)
# 1) Make a histogram of mortality rate from heart attacks
#    Added histograms for the others as well

hist(data$HeartAttack)
hist(data$HeartFailure)
hist(data$Pneumonia)

# 2) Finding the best hospital in a state

source('best.R')
best('NY', 'pneumonia')

# 3) Ranking hospitals by outcome in state

source('rankhospital.R')
rankhospital('MD', 'heart attack', 'worst')

# 4) Ranking hospitals in all states

source('rankall.R')
rankall("pneumonia")

