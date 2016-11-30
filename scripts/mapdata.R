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

# Determine Risk value
y0<-pheno%>%
  filter(Year=="2010")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y0<-na.omit(y0)
y0$Risk <- y0$Leaves - y0$Budburst
y0<-filter(y0, Risk > 0)
y0<-filter(y0, Risk < 14)

y1<-pheno%>%
  filter(Year=="2011")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y1<-na.omit(y1)
y1$Risk <- y1$Leaves - y1$Budburst
y1<-filter(y1, Risk > 0)
y1<-filter(y1, Risk < 31)

dat<-full_join(y0,y1)

y2<-pheno%>%
  filter(Year=="2012")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y2<-na.omit(y2)
y2$Risk <- y2$Leaves - y2$Budburst
y2<-filter(y2, Risk > 0)
y2<-filter(y2, Risk < 31)

dat<-full_join(dat,y2)

y3<-pheno%>%
  filter(Year=="2013")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y3<-na.omit(y3)
y3$Risk <- y3$Leaves - y3$Budburst
y3<-filter(y3, Risk > 0)
y3<-filter(y3, Risk < 31)

dat<-full_join(dat,y3)

y4<-pheno%>%
  filter(Year=="2014")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y4<-na.omit(y4)
y4$Risk <- y4$Leaves - y4$Budburst
y4<-filter(y4, Risk > 0)
y4<-filter(y4, Risk < 31)

dat<-full_join(dat,y4)

y5<-pheno%>%
  filter(Year=="2015")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y5<-na.omit(y5)
y5$Risk <- y5$Leaves - y5$Budburst
y5<-filter(y5, Risk > 0)
y5<-filter(y5, Risk < 31)

d<-full_join(dat,y5)

buds<- d %>%
  select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  filter(Budburst>=70)%>%
  arrange(species)

leaf<- d %>%
  select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  filter(Leaves>=76)%>%
  arrange(species)

df<- full_join(buds, leaf) %>%
  filter(species != "Alnus_rubra")
df$Risk<- df$Leaves - df$Budburst

map<-full_join(dat, y5) %>%
  group_by(species, Year) %>%
  arrange(species) %>%
  select(species, Year, Risk, Latitude, Longitude)
map<-na.omit(map)

map$group<-cut(map$Risk, 5, labels=c('Very Low','Low','Medium', 'High', 'Very High'))

#very.high<-map[which(map$Risk <= 30 & map$Risk >= 20), ]
#high<-map[which(map$Risk <= 19 & map$Risk >= 15), ]
#low<-map[which(map$Risk <= 9 & map$Risk >= 5), ]
#very.low<-map[which(map$Risk <= 4 & map$Risk >= 0), ]
#map$group<- c(very.high, high, med, low, very.low)

# Plot Points - Need to fix color by species
gg1 + 
    geom_point(data = map, aes(x = Longitude, y = Latitude), color = "black", size = 3) +
    geom_point(data = map, aes(x = Longitude, y = Latitude), size = 3) + 
  geom_point(fill=factor(map$group)) + theme(legend.position="none")

gg1 + geom_point(data = map, aes(Longitude, Latitude, group = group, color=group))

write.csv(map, file ="./output/map.csv", row.names = FALSE)

