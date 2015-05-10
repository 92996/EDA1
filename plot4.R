library(ggplot2);#if R>3.1.3 have to run manually selecting the text below

########FUNCTION multiplot
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

###########END OF FUNCTIONS ################

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
#Voltage
pvoltage <- ggplot(seltim, aes(rt2)) +
  geom_line(aes(y = as.numeric(Voltage)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs (x = "datetime", y = "Voltage") + 
  scale_x_datetime(breaks = as.POSIXct(c("2007-02-01 00:00:00", "2007-02-02 00:00:00", "2007-02-03 00:00:00")), labels = c("Thu", "Fri", "Sat")) +
  theme(text = element_text(size=20), axis.text.x = element_text()) +
  theme(axis.title.x = element_text(vjust = -1, size = 16)) +
  theme(axis.title.y = element_text(vjust = 2, size = 16)) 

#Global reactive power
preactive <- ggplot(seltim, aes(rt2)) +
  geom_line(aes(y = as.numeric(Global_reactive_power)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs (x = "datetime", y = "Global_reactive_power") + 
  scale_x_datetime(breaks = as.POSIXct(c("2007-02-01 00:00:00", "2007-02-02 00:00:00", "2007-02-03 00:00:00")), labels = c("Thu", "Fri", "Sat")) +
  theme(text = element_text(size=20), axis.text.x = element_text()) +
  theme(axis.title.x = element_text(vjust = -1, size = 16)) +
  theme(axis.title.y = element_text(vjust = 2, size = 16)) 

#Global active power (plot2)
pactive <- ggplot(seltim, aes(rt2)) +
  geom_line(aes(y = as.numeric(Global_active_power)))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  labs (x = "",y = "Global active power") + 
  scale_x_datetime(breaks = as.POSIXct(c("2007-02-01 00:00:00", "2007-02-02 00:00:00", "2007-02-03 00:00:00")), labels = c("Thu", "Fri", "Sat")) +
  theme(text = element_text(size=20), axis.text.x = element_text()) +
  theme(axis.title.y = element_text(vjust = 2, size = 16)) 

#sub mettering (plot3) file is better than window
psub <- ggplot(seltim, aes(rt2)) +
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
    #legend.background = element_rect(colour = "black", fill = "white", size = 0.25),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.position = c(0.71,0.822)
  ) +
  labs (x = "",y = "Energy sub metering", size = 16)

png(filename = "plot4.png", width = 480, height = 480, units = "px")
multiplot (pactive, psub, pvoltage, preactive, cols = 2)
dev.off()

