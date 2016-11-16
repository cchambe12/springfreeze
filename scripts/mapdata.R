## 9 November 2016 - Cat
## Attempt to Map out NPN data for Timeline data

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Install Packages
#install.packages(c("stringr","maps","mapdata"))
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(tidyr)

# Upload map of US
usa <- map_data("usa")
canada <- map_data("worldHires", "Canada")
mexico <- map_data("worldHires", "Mexico")
states <- map_data("state")
alaska <- subset(states, region %in% "alaska")

gg1<- NAmap <- ggplot() + geom_polygon(data = usa, 
                                       aes(x=long, y = lat, group = group), 
                                       fill = "white", 
                                       color="black") +
  geom_polygon(data = canada, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_polygon(data = mexico, aes(x=long, y = lat, group = group), 
               fill = "white", color="black") + 
  geom_polygon(data = alaska, aes(x=long, y = lat, group = group), 
                                                             fill = "white", color="black") 

# Set Working Directory
setwd("~/Documents/git/springfreeze")
phenology<-read.csv("input/condensed_phenometrics.csv",header=TRUE)
attach(phenology)

phases<-c("Budburst","Leaves")

pheno<-phenology%>%
  select(Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) %>%
  filter(Longitude != 0)

# Plot Points - Need to fix color by species
gg1 + 
    geom_point(data = pheno, aes(x = Longitude, y = Latitude), color = "black", size = 3) +
    geom_point(data = pheno, aes(x = Longitude, y = Latitude), size = 3, color=aes(species)) + 
  theme(legend.position="none")
