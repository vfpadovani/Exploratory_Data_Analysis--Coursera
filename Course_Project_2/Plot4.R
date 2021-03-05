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

# Get all the coal combustion-related data from SCC within NEI

SCC_Coal <- 
    SCC[grepl("[cC]omb (.*) [cC]oal", SCC$EI.Sector), -c(5, 6, 11, 12, 13, 14, 15)]

Coal_Emissions <- 
    NEI[SCC_Coal$SCC %in% NEI$SCC, ]

# Plot the Coal Combustion Emissions data

plot4 <-
    Coal_Emissions %>%
    select(year, Emissions) %>%
    group_by(year) %>%
    summarize(Emissions = sum(Emissions)/1000000) %>%
    ggplot() + 
    geom_line(aes(x = year,
                  y = Emissions),
              stat = "identity",
              color = "steelblue") +
    labs(title = expression('Total Coal Combustion-related Emissions of PM'[2.5]*''),
         y = "Emissions (millions of tons)",
         x = "Year") +
    theme_minimal() +
    theme(
        text = element_text("Yantramanav"),
        title = element_text(hjust = 0.5)
    ) +
    scale_x_continuous(breaks = c(1999, 2002, 2005 ,2008))

# Save

if (!file.exists("plot4.png")) {ggsave("plot4.png", plot = plot4)}