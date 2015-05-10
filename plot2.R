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
png(filename = "plot2.png", width = 480, height = 480, units = "px")
plot(Global_active_power ~ rt2, seltim, type = "l", ylab = "Global Active Power (kilowats)", xlab = "")
dev.off()

