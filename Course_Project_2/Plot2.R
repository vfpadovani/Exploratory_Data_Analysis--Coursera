# Packages
library(tidyverse)

# Loading files

if (!file.exists(c("Source_Classification_Code.rds", "summarySCC_PM25.rds"))) {
    unzip("exdata_data_NEI_data.zip")
}

if(!exists(c("SCC", "NEI"))) {
    SCC <- read_rds("Source_Classification_Code.rds")
    NEI <- read_rds("summarySCC_PM25.rds")}

# Get total PM2.5 emissions filtered by Baltimore's fips code 

Baltimore_yearEmissions <- 
    NEI %>%
    select(year, fips, Emissions) %>%
    group_by(year, fips) %>%
    summarize(Emissions = sum(Emissions)) %>%
    filter(fips == 24510)

# Plot

with(Baltimore_yearEmissions,
     barplot(height = Emissions, names.arg = year,
             xlab = "Years", main = expression('Total PM'[2.5]*' Emissions in Baltimore City, MD (tons)'), col = 'steelblue',
             ylim = c(0, 4000), las = 1))

# Save

dev.copy(png, file = "plot2.png")
dev.off()
