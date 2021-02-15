# Packages
library(tidyverse)

# Loading files

if (!file.exists(c("Source_Classification_Code.rds", "summarySCC_PM25.rds"))) {
    unzip("exdata_data_NEI_data.zip")
}

SCC <- read_rds("Source_Classification_Code.rds")
sum_SCC <- read_rds("summarySCC_PM25.rds")

