library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS('Source_Classification_Code.rds')



pdf('plot5.pdf')

data5 <-  data[data$SCC %in% SCC[grep('Vehicle', SCC$SCC.Level.Two), 1],]
data5 <- data5[data5$fips=='24510',]

g5 <- ggplot(data5, aes(x=year, y=Emissions))
g5 + geom_point(stat = 'summary', fun.y = sum) + 
  stat_summary(fun.y=sum, geom='line') + xlab('Year') + 
  ggtitle('Motor Vehicle Emission from 1999-2008 in Baltimore City') + 
  theme_minimal() + ylab('PM2.5 Emissions (tons)')

dev.off()