# Packages used

library(tidyverse)

# Load the data

if (!file.exists("household_power_consumption.txt")) {
    unzip("exdata_data_household_power_consumption.zip")
}

HPC_data <- read.csv("household_power_consumption.txt", header = T, sep = ";", na.strings = "?")

# Convert and filter date and time

str(HPC_data)

HPC_data$Date <- as.Date(HPC_data$Date, tryFormats = "%d/%m/%Y")

HPC_data <- 
    HPC_data %>%
        filter(Date >="2007-02-01" & Date <="2007-02-02")

HPC_data$DateTime <- strptime(paste(HPC_data$Date, HPC_data$Time), "%Y-%m-%d %H:%M:%S")

HPC_data <-
    HPC_data %>%
        select(DateTime, everything()) %>%
        select(-c(Date, Time))

# plotting

## Plot 1

with(HPC_data)
hist(Global_active_power, main = "Global active power", col = "red",
     xlab = "Global active power (kilowatts)", las = 1, ylim = c(0,1200))

dev.copy(png, file = "plot1.png")
dev.off()


## Plot 2

with(HPC_data)
plot(Global_active_power ~ as.POSIXct(DateTime), type = "l",
     xlab = "", ylab = "Global Active Power (kilowatts)")

dev.copy(png, file = "plot2.png")
dev.off()

## Plot 3

with(HPC_data)       
plot(Sub_metering_1 ~ as.POSIXct(DateTime), type = "l",
     ylab = "Energy sub metering", xlab = "")
lines(Sub_metering_2 ~ as.POSIXct(DateTime), col = "red")
lines(Sub_metering_3 ~ as.POSIXct(DateTime), col = "blue")
legend("topright", col = c("black", "red", "blue"),
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty = 1)

dev.copy(png, file = "plot3.png")
dev.off()

## Plot 4

par(mfrow = c(2,2), mar = c(4, 4, 1, 1), oma = c(0, 0, 0, 0))
with(HPC_data, {
    plot(Global_active_power ~ as.POSIXct(DateTime), type = "l",
         xlab = "", ylab = "Global Active Power")
    plot(Voltage ~ as.POSIXct(DateTime), type = "l",
         ylab = "Voltage", xlab = "datetime", ylim = c(234, 246))
    plot(Sub_metering_1 ~ as.POSIXct(DateTime), type = "l",
         ylab = "Energy sub metering", xlab = "")
    lines(Sub_metering_2 ~ as.POSIXct(DateTime), col = "red")
    lines(Sub_metering_3 ~ as.POSIXct(DateTime), col = "blue")
    legend("topright", col = c("black", "red", "blue"), lty = 1, lwd = 2, bty = "n",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), cex = 1/2)
    plot(Global_reactive_power ~ as.POSIXct(DateTime), type = "l", 
         xlab =  "datetime", ylim = c(0, 0.5), las = 1)
})

dev.copy(png, file = "plot4.png")
dev.off()


