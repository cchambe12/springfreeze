## 26 October 2016 - Cat
## Attempt to create a timeline chart and eventally use Stan to determine
## the effects of latitude and temperature on risk time
## This is take two with a new datasheet from the USNPN, follows bb.leafout.R and darewearm.R

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
phenology<-read.csv("input/condensed_phenometrics.csv",header=TRUE)
attach(phenology)

phases<-c("Budburst","Leaves")

pheno<-phenology%>%
  select(Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude, 
         First_Yes_Day, First_Yes_Month) %>%
  unite(species, Genus, Species, sep="_") %>%
  unite(date, First_Yes_Day, First_Yes_Month, sep="/") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) 

# Year 2010 set up data for Risk timeframe
y0<-pheno%>%
  filter(Year=="2010")%>%
  select(species, Individual_ID, Phenophase_Description, Latitude, First_Yes_DOY) %>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y0<-na.omit(y0)
y0$Risk <- y0$Leaves - y0$Budburst
y0<-filter(y0, Risk > 0)
y0<-filter(y0, Risk < 50)

buds<- y0 %>%
  select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaf<- y0 %>%
  select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)

df<- full_join(buds, leaf) %>%
  filter(species != "Alnus_rubra")
df$Risk<- df$Leaves - df$Budburst

# Make a chart at the genus level for better visual
genus<- full_join(phenology, y0)
genus<-na.omit(genus)
genus$Risk <- genus$Leaves - genus$Budburst
genus<-filter(genus, Risk > 0)
genus<-filter(genus, Risk < 200)

bud.gen<- genus %>%
  select(Genus, Budburst) %>%
  group_by(Genus)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(Genus)

leaf.gen<- genus %>%
  select(Genus, Leaves) %>%
  group_by(Genus)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(Genus)

dg<- full_join(bud.gen, leaf.gen)
dg$Risk<- dg$Leaves - dg$Budburst

ggplot((dg), aes(x=Budburst, y=Genus)) + geom_point(aes(x= dg$Budburst)) + 
  geom_segment(aes(y = Genus, yend = Genus, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=dg$Leaves)) + theme(legend.position="none") +
  geom_point(position = position_dodge(.5)) + geom_point(aes(col=Genus))


time<- df %>%
  select(species, Budburst, Leaves) %>%
  gather(date, phenophase, -species) %>%
  arrange(species)
  
# Making the chart!
plot<-ggplot((df),aes(x=Budburst,y=0)) + geom_point(aes(x = df$Budburst)) + geom_point(aes(col=species)) +
  geom_point(aes(x = df$Leaves)) + geom_point(aes(col=species)) + theme(legend.position="none") +
  geom_segment(aes(y=0,x=Budburst,xend=Leaves, yend=0))
plot

ggplot((df), aes(x=Budburst, y=species)) + geom_point(aes(x= df$Budburst)) + geom_segment(aes(y = species, yend = species,
                 x = Budburst, xend = Leaves)) + geom_point(aes(x=df$Leaves)) + theme(legend.position="none") +
  geom_point(position = position_dodge(.5)) + geom_point(aes(col=species)) + 

as.data.frame(table(df$species))
