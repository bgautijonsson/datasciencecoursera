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

# Plot 1
png(filename = 'plot1.png')
hist(subset$Global_active_power, col='red', xlab = 'Global Active Power (kilowatts)',
     main = 'Global Active Power', ylim = range(0,1200))
dev.off()

# Plot 2
png(filename = 'plot2.png')
plot(subset$datetime, subset$Global_active_power,
     type='l', xlab='', ylab='Global Active Power (kilowatts)')
dev.off()
# Plot 3
png(filename = 'plot3.png')
plot(subset$datetime, subset$Sub_metering_1,
     type='l', xlab='', ylab='Energy Sub Metering', col='black')
lines(subset$datetime, subset$Sub_metering_2, col='red')
lines(subset$datetime, subset$Sub_metering_3, col='blue')
legend('topright', legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
       lty=c(1,1,1), lwd=c(1.5,1.5,1.5), col=c('black', 'red', 'blue'))
dev.off()
# Plot 4
png(filename = 'plot4.png')
par(mar =c(5,4,3,2), mfrow=c(2,2))
## a)
plot(subset$datetime, subset$Global_active_power,
     type='l', xlab='', ylab='Global Active Power (kilowatts)')
## b)
plot(subset$datetime, subset$Voltage,
     type='l', xlab='datetime', ylab='Voltage')
## c)
plot(subset$datetime, subset$Sub_metering_1,
     type='l', xlab='', ylab='Energy Sub Metering', col='black')
lines(subset$datetime, subset$Sub_metering_2, col='red')
lines(subset$datetime, subset$Sub_metering_3, col='blue')
legend('topright', legend = c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3'),
       lty=c(1,1,1), lwd=c(1,1,1), col=c('black', 'red', 'blue'),
       bty = 'n')
## d)
plot(subset$datetime, subset$Global_reactive_power,
     type='l',xlab='datetime' ,ylab='Global_reactive_power')
dev.off()



