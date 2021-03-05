# Packages
library(tidyverse)

# Loading files

if (!file.exists(c("Source_Classification_Code.rds", "summarySCC_PM25.rds"))) {
    unzip("exdata_data_NEI_data.zip")
}

if(!exists(c("SCC", "NEI"))) {
    SCC <- read_rds("Source_Classification_Code.rds")
    NEI <- read_rds("summarySCC_PM25.rds")}

# Sum total PM'[2.5]*' emissions by year 

year_emissions <-
    NEI %>%
    select(year, Pollutant, Emissions) %>%
    group_by(year) %>%
    summarize(Emissions = sum(Emissions))

# Plot

with(year_emissions,
     barplot(height = Emissions/1000000, names.arg = year,
             xlab = "Years", main = expression('Total PM'[2.5]*' Emissions (millions of tons)'), col = 'steelblue',
             ylim = c(0, 8), las = 1))

# Save

dev.copy(png, file = "plot1.png")
dev.off()
