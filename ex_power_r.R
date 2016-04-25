make_plots <- function(){
    
    #set working directory
    setwd("R/ex_graphics")
    
    # Read File
    power <- read.csv("household_power_consumption.txt", header=T, sep=';', na.strings="?", check.names=F, stringsAsFactors=F, comment.char="", quote='\"')
    # Convert Character Date to Text Date
    power$Date <- as.Date(power$Date, format="%d/%m/%Y")
    # Subset for Feb 1, 2007 and Feb 2, 2007
    power <- subset(power, Date == as.Date("1/2/2007", format = "%d/%m/%Y") | Date == as.Date("2/2/2007", format = "%d/%m/%Y"))
    
    ## Create Histogram (Plot 1)
    png("Plot1.png", width=480, height=480)
    hist(power$Global_active_power, col = "red", xlab="Global Active Power (kilowatts)", ylab = "Frequency",main="Global Active Power")
    dev.off()
    
    ## Create GAP by Day (Plot 2)
    power$Datetime <- with(power, as.POSIXct(paste(Date, Time), format="%Y-%m-%d %H:%M:%S"))
    png("Plot2.png", width=480, height=480)
    plot(power$Global_active_power~power$Datetime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
    dev.off()
    
    ## Create Submetering (Plot 3)
    png("Plot3.png", width=480, height=480)
    plot(power$Sub_metering_1~power$Datetime, type="l", ylab="Energy sub metering", xlab="", col="black")
    lines(power$Sub_metering_2~power$Datetime, col="Red")
    lines(power$Sub_metering_3~power$Datetime, col="Blue")
    legend('topright', c("Submetering_1","Submetering_2","Submetering_3"), lty = c(1,1), col = c("black", "red", "blue"))
    dev.off()
    
    ## Create Composit (Plot 4)
    png("Plot4.png", width=480, height=480)
    par(mfrow=c(2,2))
    par(mar=c(4,4,1,1))
    # UL
    plot(power$Global_active_power~power$Datetime, type="l", ylab="Global Active Power", xlab="")
    #UR
    plot(power$Voltage~power$Datetime, type="l", ylab="Voltage", xlab="datetime")
    #LL
    plot(power$Sub_metering_1~power$Datetime, type="l", ylab="Energy sub metering", xlab="", col="black")
    lines(power$Sub_metering_2~power$Datetime, col="Red")
    lines(power$Sub_metering_3~power$Datetime, col="Blue")
    #LR
    with(power, plot(Global_reactive_power~Datetime, type="l"))
    dev.off()
    
}