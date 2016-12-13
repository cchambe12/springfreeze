###########################################################################
## 30 Nov 2016 - Cat
## Attempt to create a timeline chart and eventally use Stan to determine
## the effects of latitude and temperature on risk time
## This is take two with a new datasheet from the USNPN, follows bb.leafout.R and darewearm.R
# Using TreeSpotter data for Rethinking manuscript

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
phenology<-read.csv("input/treespotters.timeline.csv",header=TRUE)

phases<-c("Budburst","Leaves")

pheno<-phenology%>%
  select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) 

# Make dataframe that includes Risk for each species
y0<-pheno%>%
  filter(Year=="2015")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y0<-na.omit(y0)
y0$Risk <- y0$Leaves - y0$Budburst
y0<-filter(y0, Risk > 0)
y0<-filter(y0, Risk < 31)

y1<-pheno%>%
  filter(Year=="2016")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y1<-na.omit(y1)
y1$Risk <- y1$Leaves - y1$Budburst
y1<-filter(y1, Risk > 0)
y1<-filter(y1, Risk < 31)

dat<-full_join(y0,y1)

bud<- y1 %>%
  select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves<- y1 %>%
  select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)

basic<- full_join(bud, leaves)
basic$Risk<- basic$Leaves - basic$Budburst

basic$code <- reorder(basic$species, basic$Leaves)

ts.timeline<-ggplot((basic), aes(x=Budburst, y=code), stat="identity") + geom_point(aes(x= basic$Budburst)) + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic$Leaves)) + theme(legend.position="none") + geom_point(aes(col=species)) + xlab("Budburst to Leaf Out") +
  ylab("Species")
plot(ts.timeline)

ggplot((basic), aes( x= Budburst, y=Risk)) + geom_smooth(method="lm", se=FALSE) + geom_point(aes(col=species))

basic$sd<-sd(basic$Risk)
basic$mean<-mean(basic$Risk)
