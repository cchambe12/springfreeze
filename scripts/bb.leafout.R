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
  