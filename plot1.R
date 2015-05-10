if (!exists("bigmatrix")) {
  bigmatrix <- read.csv(file="household_power_consumption.txt",sep = ";", stringsAsFactors = FALSE)
  rt1 <- paste(bigmatrix$Date, bigmatrix$Time)
  rt2 <- strptime(rt1, "%d/%m/%Y %H:%M:%S")
}

memory_taken_by_matrix <- format(object.size(bigmatrix), units = "auto")

if (!exists("realtime")) {
  realtime <- cbind(rt2, bigmatrix[,3:9])
}

memory_taken_by_realtime <- format(object.size(realtime), units = "auto")

if (!exists("seltim")) {
  seltim <- subset(realtime, rt2 >= as.POSIXlt("2007-02-01 00:00:00") & rt2 <= as.POSIXlt("2007-02-02 23:59:59"))
}

plot.new()
hist(as.numeric(seltim$Global_active_power),16, title(ylab = "Frequency", col = "red"), xlab = "Global Active Power (kilowatts)", ylim=c(0,1200), main = "Global Active Power")
memory_taken_by_realtime <- format(object.size(realtime), units = "auto")
par(cex=0.8)
hist(as.numeric(seltim$Global_active_power),16, ylab = "Frequency", xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
png(filename = "plot1.png", width = 480, height = 480, units = "px")
par(cex=0.8)
hist(as.numeric(seltim$Global_active_power),16, ylab = "Frequency", xlab = "Global Active Power (kilowatts)", col = "red", main = "Global Active Power")
dev.off()

