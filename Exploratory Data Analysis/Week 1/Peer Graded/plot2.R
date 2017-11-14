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
png(filename = 'plot2.png')
plot(subset$datetime, subset$Global_active_power,
     type='l', xlab='', ylab='Global Active Power (kilowatts)')
dev.off()
