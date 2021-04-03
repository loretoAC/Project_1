#Exploratory Data Analysis - Project1

data <- read.table("household_power_consumption.txt", header=TRUE, sep=";", na.strings = "?", colClasses = c('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'))

head (data)
str(data)

data$Date <- as.Date(data$Date, "%d/%m/%Y") #type date

data <- subset(data,Date >= as.Date("2007-2-1") & Date <= as.Date("2007-2-2")) #filter data set

data <- data[complete.cases(data),] #remove incomplete data

dateTime <- paste(data$Date, data$Time) #combine both columns

dateTime <- setNames(dateTime, "DateTime") #change name

data <- data[ ,!(names(data) %in% c("Date","Time"))] #remove columns

data <- cbind(dateTime, data) #add new column and apply a format

data$dateTime <- as.POSIXct(dateTime)

#######
##    Histogram 1

hist(data$Global_active_power, breaks = 100, main="Global Active Power", 
     xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col="red")

dev.copy(png,"plot1.png", width=480, height=480)
dev.off()

#######
##    Time Series 2

plot(data$Global_active_power~data$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")

dev.copy(png,"plot2.png", width=480, height=480)
dev.off()

#######
##    Time Series 3
with(data, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))

dev.copy(png,"plot3.png", width=480, height=480)
dev.off()


########
##    Multipanel

par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(data, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})

dev.copy(png,"plot4.png", width=480, height=480)
dev.off()

