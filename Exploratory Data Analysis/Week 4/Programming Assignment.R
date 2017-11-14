library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS('Source_Classification_Code.rds')


# 1)
years <- unique(data$year)
data_by_year <- split(data, data$year)
totals <- sapply(data_by_year, function(data) sum(data$Emissions))
plot(years, totals, type = 'b', ylab = 'PM2.5 Emission (tons)',
     xlab = 'Year', main = 'Total Emission in USA Over the Years', xlim = c(1998, 2008))

# 2)

balt <- data[data$fips=='24510',]
balt_by_year <- split(balt, balt$year)
balt_totals <- sapply(balt_by_year, function(data) sum(data$Emissions))
plot(years, balt_totals, type = 'b', ylab = 'PM2.5 Emission (tons)',
     xlab = 'Year', main = 'Emission in Baltimore Over the Years', 
     xlim = c(1998, 2008))

# 3)

unique(SCC$Data.Category)
select3 <- SCC[SCC$Data.Category %in% c('Point', 'Nonpoint', 'Onroad', 'Nonroad'),]
data3 <- data[data$SCC %in% select3$SCC,]
data3 <- data3[data3$fips=='24510',]

g3 <- ggplot(data3, aes(x=year, y=Emissions))
g3 + geom_point(stat = 'summary', fun.y = sum) + facet_grid(facets = . ~ type) + 
  stat_summary(fun.y=sum, geom='line') + xlab('Year') +
    ggtitle('Emissions Sorted By Type from 1999-2008 in Baltimore City') + 
    theme_bw() + ylab('PM2.5 Emissions (tons)')


# 4) 

coal_data <- data[data$SCC %in% SCC[grep('[Cc]oal', SCC$EI.Sector), 1],]

g4 <- ggplot(coal_data, aes(x=year, y=Emissions))
g4 + geom_point(stat = 'summary', fun.y = sum) + 
  stat_summary(fun.y=sum, geom='line') +
    ggtitle('Coal Combustion-Related Emission From 1999-2008') +
    xlab('Year') + ylab('PM2.5 Emissions (tons)') + theme_minimal()

# 5) 

unique(SCC$SCC.Level.One)

data5 <-  data[data$SCC %in% SCC[grep('Vehicle', SCC$SCC.Level.Two), 1],]
data5 <- data5[data5$fips=='24510',]

g5 <- ggplot(data5, aes(x=year, y=Emissions))
g5 + geom_point(stat = 'summary', fun.y = sum) + 
  stat_summary(fun.y=sum, geom='line') + xlab('Year') + 
    ggtitle('Motor Vehicle Emission from 1999-2008 in Baltimore City') + 
    theme_minimal() + ylab('PM2.5 Emissions (tons)')


# 6)
str(data6)
data6 <- data[data$SCC %in% SCC[grep('Vehicle', SCC$SCC.Level.Two), 1],]
data6 <- data6[data6$fips=='24510' | data6$fips== '06037',]
data6[data6$fips=='24510', 1] <- 'Baltimore City'
data6[data6$fips=='06037', 1] <- 'Los Angeles County'
g6 <- ggplot(data6, aes(x=year, y=Emissions))
g6 + geom_point(stat = 'summary', fun.y = sum) + 
    facet_grid(facets = . ~ fips) + stat_summary(fun.y=sum, geom='line') + 
    ylab('PM2.5 Emissions (tons)') + xlab('Year') + theme_bw() + 
    ggtitle('Comparison of Motor Vehicle Emission in Two Cities')
  