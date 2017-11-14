library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS('Source_Classification_Code.rds')

pdf('plot3.pdf')


select3 <- SCC[SCC$Data.Category %in% c('Point', 'Nonpoint', 'Onroad', 'Nonroad'),]
data3 <- data[data$SCC %in% select3$SCC,]
data3 <- data3[data3$fips=='24510',]

g3 <- ggplot(data3, aes(x=year, y=Emissions))
g3 + geom_point(stat = 'summary', fun.y = sum) + facet_grid(facets = . ~ type) + 
  stat_summary(fun.y=sum, geom='line') + xlab('Year') +
  ggtitle('Emissions Sorted By Type from 1999-2008 in Baltimore City') + 
  theme_bw() + ylab('PM2.5 Emissions (tons)')

dev.off()
