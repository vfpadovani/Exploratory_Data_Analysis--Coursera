# Packages used

library(tidyverse)
library(patchwork)
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

### *"qui", "sex" & "sab" are Portuguese for "thu", "fri" & "sat"*

## Plot 3

colors <- c("Sub_metering_1" = "black", "Sub_metering_2" = "red", "Sub_metering_3" = "steelblue")

plot3 <-
    ggplot(HPC_data, aes(x = as.POSIXct(DateTime))) +
        geom_line(aes(y = Sub_metering_1,
                      colour = "Sub_metering_1")) +
        geom_line(aes(y = Sub_metering_2,
                  colour = "Sub_metering_2")) +
        geom_line(aes(y = Sub_metering_3,
                  colour = "Sub_metering_3")) +
        theme_minimal() +
        theme(text = element_text("Overpass"),
              strip.text = element_text(face = 'bold',
                                        hjust = .5),
              panel.background = element_rect(),
              legend.position = c(1, 1),
              legend.justification = c("right", "top"),
              legend.box.background = element_rect(),
              legend.box.just = "right",
              legend.background = element_blank()
        )+
        labs(x = element_blank(),
             y = "Energy sub metering",
             colour = "Legend") +
        scale_x_datetime(date_labels = "%a",
                         breaks = "1 day") +
        scale_color_manual(values = colors)

if (!file.exists("plot3.png")) {ggsave("plot3.png", plot = plot3)}        

## Plot 4

plot4.1 <-
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
         y = "Global Active Power") +
    scale_x_datetime(date_labels = "%a",
                     breaks = "1 day")

plot4.2 <-
    ggplot(HPC_data) +
    geom_line(aes(y = Voltage,
                  x = as.POSIXct(DateTime))) +
    theme_minimal() +
    theme(text = element_text("Overpass"),
          strip.text = element_text(face = 'bold',
                                    hjust = .5),
          panel.background = element_rect()
    )+
    labs(x = "datetime",
         y = "Voltage") +
    scale_x_datetime(date_labels = "%a",
                     breaks = "1 day") +
    scale_y_continuous(breaks = c(234, 238, 242, 246))    

plot4.3 <-
    ggplot(HPC_data, aes(x = as.POSIXct(DateTime))) +
    geom_line(aes(y = Sub_metering_1,
                  colour = "Sub_metering_1")) +
    geom_line(aes(y = Sub_metering_2,
                  colour = "Sub_metering_2")) +
    geom_line(aes(y = Sub_metering_3,
                  colour = "Sub_metering_3")) +
    theme_minimal() +
    theme(text = element_text("Overpass"),
          strip.text = element_text(face = 'bold',
                                    hjust = .5),
          panel.background = element_rect(),
          legend.position = c(1, 1),
          legend.justification = c("right", "top"),
          legend.box.background = element_rect(),
          legend.box.just = "right",
          legend.background = element_blank(),
          legend.key.size = unit(.25, "cm")
    )+
    labs(x = element_blank(),
         y = "Energy sub metering",
         colour = "Legend") +
    scale_x_datetime(date_labels = "%a",
                     breaks = "1 day") +
    scale_color_manual(values = colors)

plot4.4 <-
    ggplot(HPC_data) +
    geom_line(aes(y = Global_reactive_power,
                  x = as.POSIXct(DateTime))) +
    theme_minimal() +
    theme(text = element_text("Overpass"),
          strip.text = element_text(face = 'bold',
                                    hjust = .5),
          panel.background = element_rect()
    )+
    labs(x = "datetime",
         y = "Global Reactive Power") +
    scale_x_datetime(date_labels = "%a",
                     breaks = "1 day")

plot4 <- (plot4.1|plot4.2) / (plot4.3|plot4.4)

if (!file.exists("plot4.png")) {ggsave("plot4.png", plot = plot4)} 