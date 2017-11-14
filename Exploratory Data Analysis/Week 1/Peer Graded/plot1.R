library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)

data <- read.table('household_power_consumption.txt', header=TRUE, na.strings = '?',
                   sep = ';', stringsAsFactors = FALSE)
tib <- as_data_frame(data)

tib$datetime <- paste(tib$Date, tib$Time, sep=':')
tib$datetime <- strptime(tib$datetime, format = '%d/%m/%Y:%H:%M:%S')
tib$datetime

subset <- subset(tib, format(datetime, '%Y-%m-%d') %in% c('2007-02-01',
                                                          '2007-02-02'))
png(filename = 'plot1.png')
hist(subset$Global_active_power, col='red', xlab = 'Global Active Power (kilowatts)',
     main = 'Global Active Power', ylim = range(0,1200))
dev.off()
