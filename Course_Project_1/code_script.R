# Packages used

library(tidyverse)
library(lubridate)
library(extrafont)

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

weekdays(HPC_data$DateTime, abbreviate = T)

# plotting

## Plot 1

plot1 <- 
    ggplot(HPC_data) +
        geom_histogram(aes(Global_active_power),
                       colour = "black",
                       fill = "red",
                       bins = 18
                      ) +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.4),
            text = element_text("Overpass"),
            strip.text = element_text(face = 'bold',
                                      hjust = 2),
            panel.grid = element_blank()
        )+
        labs(title = "Global Active Power",
             x = "Global Active Power (kilowatts)",
             y = "Frequency") +
        scale_x_continuous(breaks = c(0, 2, 4, 6)) +
        scale_y_continuous(breaks = c(0, 200, 400, 600, 800, 1000, 1200))

if (!file.exists("plot1.png")) {ggsave("plot1.png", plot = plot1)}
    

## Plot 2

plot2 <-
    ggplot(HPC_data) +
        geom_line(aes(y = Global_active_power,
                      x = as.POSIXct(DateTime))) +
        theme_minimal() +
        theme(text = element_text("Overpass"),
              strip.text = element_text(face = 'bold',
                                        hjust = .5),
              panel.background = element_rect()
              )+
        labs(x = element_blank(),
             y = "Global Active Power (kilowatts)") +
        scale_x_datetime(date_labels = "%a",
                         breaks = "1 day")

if (!file.exists("plot2.png")) {ggsave("plot2.png", plot = plot2)}

#### "qui", "sex" & "sab" are portuguese for "thu", "fri" & "sat".

## Plot 3



## Plot 4

barplot(HPC_data$Global_active_power,
        ylab = "Global Active Power (kilowatts)",
        xlab = "Weekdays")


