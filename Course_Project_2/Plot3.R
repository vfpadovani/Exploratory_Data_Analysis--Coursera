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

# Get total PM2.5 emissions in Baltimore City, MD.
 
plot3 <-
    NEI %>%
    select(year, fips, Emissions, type) %>%
    group_by(fips, year, type) %>%
    summarize(Emissions = sum(Emissions)) %>%
    filter(fips == 24510) %>%
    ggplot() +
    geom_line(aes(x = year,
                  y = Emissions),
              color = "steelblue",
              stat = 'identity')+
# Plot on a facet wrap by type, with free y scales so it's better for seeing the outcome for each type.
    facet_wrap(~ type, scales = 'free_y') +
    labs(title = expression('Total PM'[2.5]*' Emissions in Baltimore City, MD per type'),
         y = "Emissions (tons)",
         x = "Year") +
    scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) +
    theme_minimal() +
    theme(text = element_text('Yantramanav'),
          strip.text = element_text(face = 'bold',
                                    hjust = 0),
          panel.grid.major = element_line('white',
                                          size = 0.5))

# Save

if (!file.exists("plot3.png")) {ggsave("plot3.png", plot = plot3)}
