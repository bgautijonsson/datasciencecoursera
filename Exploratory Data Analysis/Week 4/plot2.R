library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS('Source_Classification_Code.rds')

pdf('plot2.pdf')

balt <- data[data$fips=='24510',]
balt_by_year <- split(balt, balt$year)
balt_totals <- sapply(balt_by_year, function(data) sum(data$Emissions))
plot(years, balt_totals, type = 'b', ylab = 'PM2.5 Emission (tons)',
     xlab = 'Year', main = 'Emission in Baltimore Over the Years', 
     xlim = c(1998, 2008))

dev.off()