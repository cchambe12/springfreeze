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
library(arm)
library(data.table)
# Set Working Directory
setwd("~/Documents/git/springfreeze")
phenology<-read.csv("input/treespotters.timeline.csv",header=TRUE)

phases<-c("Budburst","Leaves")

pheno<-phenology%>%
  dplyr::select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) 

# Make dataframe that includes Risk for each species
y1<-pheno%>%
  filter(Year=="2016")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y1<-na.omit(y1)
y1$Risk <- y1$Leaves - y1$Budburst
y1<-filter(y1, Risk > 0)
y1<-ungroup(y1)

y1$mean<-ave(y1$Budburst, y1$species)
y1$sd<-ave(y1$Budburst, y1$species, FUN=sd)
y1$mean.leaf<-ave(y1$Leaves, y1$species)
y1$sd.leaf<-ave(y1$Leaves, y1$species, FUN=sd)
y<- y1 %>%
  dplyr::select(species, Budburst, Leaves, Risk, mean, sd, mean.leaf, sd.leaf) %>%
  gather(Phenophase, DOY, -species, -Risk, -mean, -sd, -mean.leaf, -sd.leaf)
y<-y %>%
  group_by(species, mean, Phenophase)%>%
  arrange(species)%>%
  filter(row_number()==1)%>%
  ungroup(y)

y3 = melt(setDT(y), 
           measure.vars = patterns("mean", "mean.leaf"), 
           variable.name = "Mean")
y3[, Mean := factor(Mean, labels = c("bud","leaf"))]
y3<-rename(y3, Budburst=value1, Leaves=value2)
y3<- y3 %>%
  dplyr::select(-Mean)%>%
  arrange(Phenophase)%>%
  gather(pheno, Mean, -species, -Risk, -Phenophase, -DOY, -sd, -sd.leaf)
y3$Phenophase<- ifelse(y3$Phenophase==y3$pheno, y3$Phenophase, NA)
y3<-na.omit(y3)
y3 <- y3[-c(12:22), ]
y3<- dplyr::select(y3, -pheno)

y2 = melt(setDT(y3), 
          measure.vars = patterns("sd", "sd.leaf"), 
          variable.name = "sd")
y2[, sd := factor(sd, labels = c("bud","leaf"))]
y2<-rename(y2, Budburst=value1, Leaves=value2)
y2<- y2 %>%
  dplyr::select(-sd)%>%
  arrange(Phenophase)%>%
  gather(pheno, sd, -species, -Risk, -Phenophase, -DOY, -Mean)
y2$Phenophase<- ifelse(y2$Phenophase==y2$pheno, y2$Phenophase, NA)
y2<-na.omit(y2)
y2 <- y2[-c(12:22), ]
y2<- dplyr::select(y2, -pheno)

bud<- y1 %>%
  dplyr::select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves<- y1 %>%
  dplyr::select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)

sd<- y1 %>%
  dplyr::select(species, sd, sd.leaf)

basic<- full_join(bud, leaves)
basic$Risk<- basic$Leaves - basic$Budburst
basic<-full_join(basic, sd)

basic$code <- reorder(basic$species, basic$Budburst)

#df<-basic %>%
  #gather(Phenophase, DOY, -Risk, -species)
#df$sd<- df%>%
  #group_by(species)%>%
  #summarise_each(funs(sd), DOY)


ts.timeline<-ggplot((basic), aes(x=Budburst, y=code), stat="identity") + geom_point() + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic$Leaves)) + theme(legend.position="none") + geom_point() + xlab("Budburst to Leaf Out") +
  ylab("Species") +geom_errorbarh(aes(xmin=Budburst-sd, xmax=Budburst+sd, col="coral"), height=.0) + 
  geom_errorbarh(aes(xmin=Leaves-sd.leaf, xmax=Leaves+sd.leaf, col="forestgreen"), height=.0)
plot(ts.timeline)

ts<-ggplot((y2), aes(x=species, y=DOY)) + geom_point(aes(col=Phenophase)) + 
  xlab("Day of Year") + ylab("Species") + geom_errorbar(aes(ymin=DOY-sd, ymax=DOY+sd, col=Phenophase), width=.0)
plot(ts)


ggplot((basic), aes( x= Budburst, y=Risk)) + geom_smooth(method="lm", se=FALSE) + geom_point(aes(col=species))

lmodel<-lm(Risk~Budburst,data=basic)
lme1<-lm(Risk~species, data=y2)
display(lme1)
