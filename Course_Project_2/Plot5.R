# Packages
library(tidyverse)
library(extrafont)

# Loading files

if (!file.exists(c("Source_Classification_Code.rds", "summarySCC_PM25.rds"))) {
    unzip("exdata_data_NEI_data.zip")
}

if(!exists(c("SCC", "NEI"))) {
    SCC <- read_rds("Source_Classification_Code.rds")
    NEI <- read_rds("summarySCC_PM25.rds")}

# Get all the Motor Vehicles Sources data within NEI

SCC_MotorV <- 
    SCC[grepl("[mM]obile (.*) ([vV]ehicle|[aA]ircraft|[vV]essel|[lL]ocomotives)", SCC$EI.Sector), -c(5, 6, 11, 12, 13, 14, 15)]

MV_Emissions <- 
    NEI[SCC_MotorV$SCC %in% NEI$SCC, ]

# Plot the Motor Vehicles data filtered by Baltimore's fips 

plot5 <-
    MV_Emissions %>%
    select(year, fips, Emissions) %>%
    group_by(fips, year) %>%
    summarize(Emissions = sum(Emissions)) %>%
    filter(fips == 24510) %>%
    ggplot() +
    geom_line(aes(x = year,
                  y = Emissions),
              stat = "identity",
              color = "steelblue") +
    labs(title = expression('Total Motor Vehicles Emissions of PM'[2.5]*' in Baltimore City, MD'),
         y = "Emissions (tons)",
         x = "Year") +
    theme_minimal() +
    theme(
        text = element_text("Yantramanav"),
        title = element_text(hjust = 0.5)
    ) +
    scale_x_continuous(breaks = c(1999, 2002, 2005 ,2008))

# Save

if (!file.exists("plot5.png")) {ggsave("plot5.png", plot = plot5)}
