## Downloaded USNPN Data to Establish Timeline from Bud Burst
## to Leaf Out for various Deciduous Broadleaf species around the US
## 12 October 2016 - Cat

# The core aim of this script is to establish a timeline for last spring freeze risk

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
phenology<-read.csv("input/individual_phenometrics_data.csv",header=TRUE, sep=",")
attach(phenology)

phases<-c("Budburst","Leaves")

pheno<-phenology%>%
  select(Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude)%>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year)
  
spp<-as.data.frame(table(phenology$species))

st<-pheno%>%
  group_by(species, Individual_ID, Phenophase_Description, Year)%>%
  arrange(Year) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
st<-na.omit(d)
st$Risk <- st$Leaves - st$Budburst
st<-filter(st, Risk > 0)
st<-filter(st, Risk < 200)

d<-pheno%>%
  filter(Year=="2010")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
d<-na.omit(d)
d$Risk <- d$Leaves - d$Budburst
d<-filter(d, Risk > 0)
d<-filter(d, Risk < 200)
sp<-as.data.frame(table(d$species))

## Determine the average number of days between for each genus
df<- d %>%
  select(species, Risk) %>%
  group_by(species) %>%
  summarise_each(funs(mean), Risk) %>%
  arrange(species)

ggplot(df,aes(x=species,y=Risk, fill=factor(species))) +
  geom_bar(stat = "identity" , position="dodge") +
  xlab("Species")+ylab("Level of Risk") 

genus<- full_join(phenology, d)
genus<-na.omit(genus)
genus$Risk <- genus$Leaves - genus$Budburst
genus<-filter(genus, Risk > 0)
genus<-filter(genus, Risk < 200)

gen.dat<- genus %>%
  select(Genus, species, Risk, Individual_ID) %>%
  group_by(species, Risk, Individual_ID)%>%
  filter(row_number()==1)
  
gen<- gen.dat%>%
  select(Genus, Risk) %>%
  group_by(Genus) %>%
  summarise_each(funs(mean), Risk)%>%
  arrange(Genus)

ggplot(gen,aes(x=Genus,y=Risk, fill=factor(Genus))) +
  geom_bar(stat = "identity" , position="dodge") +
  xlab("Genus")+ylab("Level of Risk") 

## Determine average date bud burst and leaf out for each genus
timeline<- genus %>%
  select(Genus, Individual_ID, species, Budburst, Leaves) %>%
  group_by(Genus, Individual_ID)%>%
  filter(row_number()==1)
bb<- timeline %>%
  select(Genus, Budburst) %>%
  group_by(Genus) %>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(Genus)
leaf<- timeline %>%
  select(Genus, Leaves) %>%
  group_by(Genus) %>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(Genus)
map<-full_join(bb,leaf)

## Make a timeline!!
time<-ggplot(map,aes(x=,y=0))
time+geom_segment(aes(y=0,x=Budburst,xend=Leaves, yend=0), color=aes(Genus))
time+geom_segment(x=Budburst,xend=Leaves,y=0,yend=2,color=aes(Genus),size=1) 
 #drawing the actual arrow

