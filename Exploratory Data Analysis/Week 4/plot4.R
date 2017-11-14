library(tidyr)
library(dplyr)
library(ggplot2)

data <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS('Source_Classification_Code.rds')

pdf('plot4.pdf')

coal_data <- data[data$SCC %in% SCC[grep('[Cc]oal', SCC$EI.Sector), 1],]

g4 <- ggplot(coal_data, aes(x=year, y=Emissions))
g4 + geom_point(stat = 'summary', fun.y = sum) + 
  stat_summary(fun.y=sum, geom='line') +
  ggtitle('Coal Combustion-Related Emission From 1999-2008') +
  xlab('Year') + ylab('PM2.5 Emissions (tons)') + theme_minimal()

dev.off()