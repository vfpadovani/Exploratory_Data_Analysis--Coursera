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

# Get all the Motor Vehicles sources data within NEI

SCC_MotorV <- 
    SCC[grepl("[mM]obile (.*) ([vV]ehicle|[aA]ircraft|[vV]essel|[lL]ocomotives)", SCC$EI.Sector), -c(5, 6, 11, 12, 13, 14, 15)]

MV_Emissions <- 
    NEI[SCC_MotorV$SCC %in% NEI$SCC, ]

# Set the facets labels 

fips_labels <- c("Los Angeles County, CA", "Balimore City, MD")
names(fips_labels) <- c("06037", "24510")

# Plot the Motor vehicles data filtered by Los Angeles and Baltimore's fips

plot6 <- 
    MV_Emissions %>%
    select(year, fips, Emissions) %>%
    group_by(fips, year) %>%
    summarize(Emissions = sum(Emissions)) %>%
    filter(fips %in% c("24510", "06037")) %>%
    ggplot() +
    geom_line(aes(x = year,
                  y = Emissions),
              stat = "identity",
              color = "steelblue") +
# Plot in facets so it's better for comparing the data for each city, used free y scale for a clearer view   
    facet_wrap(~ fips, scales = "free_y",
               labeller = labeller(fips = fips_labels)) +
    labs(title = expression('Total Motor Vehicles Emissions of PM'[2.5]*''),
         y = "Emissions (tons)",
         x = "Year") +
    theme_minimal() +
    theme(
        text = element_text("Yantramanav"),
        title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold",
                                  hjust = 0)
    ) +
    scale_x_continuous(breaks = c(1999, 2002, 2005 ,2008))

# Save

if (!file.exists("plot6.png")) {ggsave("plot6.png", plot = plot6)}
