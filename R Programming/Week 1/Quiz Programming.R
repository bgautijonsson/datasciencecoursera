x <- 1:4
y <- 2

class(x+y)




# Question 9
x <- c(3, 5, 1, 10, 12, 6)
x[x<6] <- 0

x <- c(3, 5, 1, 10, 12, 6)
x[x<6] = 0

x <- c(3, 5, 1, 10, 12, 6)
x[x %in% 1:5] <- 0

x <- c(3, 5, 1, 10, 12, 6)
x[x<=5] <- 0

# Questions 10+
Table <- read.csv("hw1_data.csv", header=TRUE)
colnames(Table)
Table[1:2,]

nrow(Table)

Table[152:153,]

Table[47,]

sum(is.na(Table$Ozone))

mean(na.omit(Table$Ozone))

Table.subset <- Table[Table$Ozone > 31,]

Table.subset <- Table.subset[Table$Temp > 90,]

Table.subset <- na.omit(Table.subset)

mean(Table.subset$Solar.R)

Table.19 <-Table[Table$Month == 6,]
mean(Table.19$Temp)

Table.20 <- na.omit(Table[Table$Month == 5,])
max(Table.20$Ozone)

