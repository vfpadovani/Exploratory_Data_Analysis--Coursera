# Packages used

library(lubridate)
library(tidyverse)

# Load the data

if (!file.exists("Electric Power Consumption data")) {
    unzip("exdata_data_household_power_consumption.zip")
}

HPC_data <- read.csv("household_power_consumption.txt", sep = ";")

# Convert and filter date 

str(HPC_data)

HPC_data$Date <- as.Date(HPC_data$Date, tryFormats = "%d/%m/%Y")

HPC_data <- 
    HPC_data %>%
        filter(Date == c("2007-02-01", "2007-02-02"))


    