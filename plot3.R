library("ggplot2") ;#if R>3.1.3 have to run manually selecting the text below

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

ggplot(seltim, aes(rt2)) +
  geom_line(aes(y = as.numeric(Sub_metering_1), colour = "Sub_metering_1")) +
  geom_line(aes(y = as.numeric(Sub_metering_2), colour = "Sub_metering_2")) +
  geom_line(aes(y = Sub_metering_3, colour = "Sub_metering_3")) +
  scale_colour_manual(values=c("black", "red", "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank()) +
  scale_x_datetime(breaks = as.POSIXct(c("2007-02-01 00:00:00", "2007-02-02 00:00:00", "2007-02-03 00:00:00")), labels = c("Thu", "Fri", "Sat")) + 
  theme(
    axis.text = element_text(size = 14),
    legend.key = element_rect(size = 0),
    legend.background = element_rect(colour = "black", fill = "white", size = 0.25),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = c(0.880,0.888)
  ) +
  labs (x = "",y = "Energy sub metering", size = 16)

png(filename = "plot3.png", width = 480, height = 480, units = "px")
ggplot(seltim, aes(rt2)) +
  geom_line(aes(y = as.numeric(Sub_metering_1), colour = "Sub_metering_1")) +
  geom_line(aes(y = as.numeric(Sub_metering_2), colour = "Sub_metering_2")) +
  geom_line(aes(y = Sub_metering_3, colour = "Sub_metering_3")) +
  scale_colour_manual(values=c("black", "red", "blue")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank()) +
  scale_x_datetime(breaks = as.POSIXct(c("2007-02-01 00:00:00", "2007-02-02 00:00:00", "2007-02-03 00:00:00")), labels = c("Thu", "Fri", "Sat")) + 
  theme(
    axis.text = element_text(size = 14),
    legend.key = element_rect(size = 0),
    legend.background = element_rect(colour = "black", fill = "white", size = 0.25),
    legend.title = element_blank(),
    legend.text = element_text(size = 14),
    legend.position = c(0.845,0.925)
  ) +
  labs (x = "",y = "Energy sub metering", size = 16)
dev.off()

