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


# Plot 1

year_emissions <-
    NEI %>%
        select(year, Pollutant, Emissions) %>%
        group_by(year) %>%
        summarize(Emissions = sum(Emissions))

with(year_emissions,
barplot(height = Emissions/1000000, names.arg = year,
        xlab = "Years", main = expression('Total PM'[2.5]*' Emissions (millions of tons)'), col = 'steelblue',
        ylim = c(0, 8), las = 1))


dev.copy(png, file = "plot1.png")
dev.off()

# Plot 2

Baltimore_yearEmissions <- 
    NEI %>%
        select(year, fips, Emissions) %>%
        group_by(year, fips) %>%
        summarize(Emissions = sum(Emissions)) %>%
        filter(fips == 24510)

with(Baltimore_yearEmissions,
     barplot(height = Emissions, names.arg = year,
             xlab = "Years", main = expression('Total PM'[2.5]*' Emissions in Baltimore City, MD (tons)'), col = 'steelblue',
             ylim = c(0, 4000), las = 1))

dev.copy(png, file = "plot2.png")
dev.off()

# Plot 3

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


if (!file.exists("plot3.png")) {ggsave("plot3.png", plot = plot3)}
    
    
# Plot 4

SCC_Coal <- 
    SCC[grepl("[cC]omb (.*) [cC]oal", SCC$EI.Sector), -c(5, 6, 11, 12, 13, 14, 15)]

Coal_Emissions <- 
    NEI[SCC_Coal$SCC %in% NEI$SCC, ]

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

if (!file.exists("plot4.png")) {ggsave("plot4.png", plot = plot4)}

# Plot 5

SCC_MotorV <- 
    SCC[grepl("[mM]obile (.*) ([vV]ehicle|[aA]ircraft|[vV]essel|[lL]ocomotives)", SCC$EI.Sector), -c(5, 6, 11, 12, 13, 14, 15)]

MV_Emissions <- 
    NEI[SCC_MotorV$SCC %in% NEI$SCC, ]

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
        labs(title = expression('Total Motor Vehicles Emissions of PM'[2.5]*' in Baltimore City, MV'),
             y = "Emissions (tons)",
             x = "Year") +
        theme_minimal() +
        theme(
            text = element_text("Yantramanav"),
            title = element_text(hjust = 0.5)
        ) +
        scale_x_continuous(breaks = c(1999, 2002, 2005 ,2008))

if (!file.exists("plot5.png")) {ggsave("plot5.png", plot = plot5)}
    
# Plot 6

fips_labels <- c("Los Angeles County, CA", "Balimore City, MD")
names(fips_labels) <- c("06037", "24510")

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

if (!file.exists("plot6.png")) {ggsave("plot6.png", plot = plot6)}

