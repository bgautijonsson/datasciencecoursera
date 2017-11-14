library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS('Source_Classification_Code.rds')

pdf('plot6.pdf')

data6 <- data[data$SCC %in% SCC[grep('Vehicle', SCC$SCC.Level.Two), 1],]
data6 <- data6[data6$fips=='24510' | data6$fips== '06037',]
data6[data6$fips=='24510', 1] <- 'Baltimore City'
data6[data6$fips=='06037', 1] <- 'Los Angeles County'
g6 <- ggplot(data6, aes(x=year, y=Emissions))
g6 + geom_point(stat = 'summary', fun.y = sum) + 
  facet_grid(facets = . ~ fips) + stat_summary(fun.y=sum, geom='line') + 
  ylab('PM2.5 Emissions (tons)') + xlab('Year') + theme_bw() + 
  ggtitle('Comparison of Motor Vehicle Emission in Two Cities')

dev.off()