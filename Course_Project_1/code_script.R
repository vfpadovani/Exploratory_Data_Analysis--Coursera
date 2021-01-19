# Packages used

# Load the data

if (!file.exists("Electric Power Consumption data")) {
    unzip("exdata_data_household_power_consumption.zip")
}

HPC_data <- read.csv("household_power_consumption.txt", sep = ";")

# 