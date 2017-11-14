library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS('Source_Classification_Code.rds')

pdf('plot1.pdf')

years <- unique(data$year)
data_by_year <- split(data, data$year)
totals <- sapply(data_by_year, function(data) sum(data$Emissions))
plot(years, totals, type = 'b', ylab = 'PM2.5 Emission (tons)',
     xlab = 'Year', main = 'Total Emission in USA Over the Years', xlim = c(1998, 2008))

dev.off()