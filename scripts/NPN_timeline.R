## Attempting to find more compelling data from NPN
# Cat - 23 March 2017
## After working with Tree Spotters data, I have realized it isn't informative enough
# Observer bias is skewing data results so we need a bigger dataset
# hoping to integrate John O'Keefes data from 2010 and 2014 and see if 
# we can find comparisons or more interesting results

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
ten<-read.csv("input/NPN_2016.csv",header=TRUE)
fourteen<-read.csv("input/NPN_2014.csv",header=TRUE)

phases<-c("Budburst","Leaves")

pheno10<-ten%>%
  dplyr::select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude, NumYs_in_Series) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) %>%
  filter(NumYs_in_Series>1)

df<-pheno10%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
df<-na.omit(df)
df$Risk <- df$Leaves - df$Budburst
df<-filter(df, Risk > 0)
df<-ungroup(df)

df$mean<-ave(df$Budburst, df$species)
df$sd<-ave(df$Budburst, df$species, FUN=sd)
df$mean.leaf<-ave(df$Leaves, df$species)
df$sd.leaf<-ave(df$Leaves, df$species, FUN=sd)

y<- df %>%
  dplyr::select(species, Budburst, Leaves, Risk, mean, sd, mean.leaf, sd.leaf) %>%
  gather(Phenophase, DOY, -species, -Risk, -mean, -sd, -mean.leaf, -sd.leaf)
y<-y %>%
  group_by(species, mean, Phenophase)%>%
  arrange(species)%>%
  filter(row_number()==1)%>%
  ungroup(y)


bud<- df %>%
  dplyr::select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves<- df %>%
  dplyr::select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)

sd<- y %>%
  dplyr::select(species, sd, sd.leaf)

basic<- full_join(bud, leaves)
basic$Risk<- basic$Leaves - basic$Budburst
basic<-full_join(basic, sd)
basic$code <- reorder(basic$species, basic$Budburst)

ts.timeline<-ggplot((basic), aes(x=Budburst, y=code), stat="identity") + geom_point(aes(x= basic$Budburst)) + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic$Leaves)) + theme(legend.position="none") + geom_point() + xlab("Budburst to Leaf Out") +
  ylab("Species") +geom_errorbarh(aes(xmin=Budburst-sd, xmax=Budburst+sd, col="coral"), height=.0) + 
  geom_errorbarh(aes(xmin=Leaves-sd.leaf, xmax=Leaves+sd.leaf, col="forestgreen"), height=.0)
plot(ts.timeline)

y$code <- reorder(y$species, y$DOY)

ts<-ggplot((y), aes(x=code, y=DOY)) + geom_point(aes(col=Phenophase)) + 
  xlab("Day of Year") + ylab("Species") + geom_errorbar(aes(ymin=DOY-sd, ymax=DOY+sd, col=Phenophase), width=.0)
plot(ts)

##### 2014 ##########
pheno14<-fourteen%>%
  dplyr::select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude, NumYs_in_Series) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) %>%
  filter(NumYs_in_Series>1)

df14<-pheno14%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
df14<-na.omit(df14)
df14$Risk <- df14$Leaves - df14$Budburst
df14<-filter(df14, Risk > 0)
df14<-ungroup(df14)

df14$mean<-ave(df14$Budburst, df14$species)
df14$sd<-ave(df14$Budburst, df14$species, FUN=sd)
df14$mean.leaf<-ave(df14$Leaves, df14$species)
df14$sd.leaf<-ave(df14$Leaves, df14$species, FUN=sd)

y14<- df14 %>%
  dplyr::select(species, Budburst, Leaves, Risk, mean, sd, mean.leaf, sd.leaf) %>%
  gather(Phenophase, DOY, -species, -Risk, -mean, -sd, -mean.leaf, -sd.leaf)
y14<-y14 %>%
  group_by(species, mean, Phenophase)%>%
  arrange(species)%>%
  filter(row_number()==1)%>%
  ungroup(y14)


bud14<- df14 %>%
  dplyr::select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves14<- df14 %>%
  dplyr::select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)
sd14<- df14 %>%
  dplyr::select(species, sd, sd.leaf)


basic14<- full_join(bud14, leaves14)
basic14<-full_join(basic14, sd14)
basic14$Risk<- basic14$Leaves - basic14$Budburst
basic14$code <- reorder(basic14$species, basic14$Budburst)

ts.timeline14<-ggplot((basic14), aes(x=Budburst, y=code), stat="identity") + geom_point() + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic14$Leaves)) + theme(legend.position="none") + geom_point() + xlab("Budburst to Leaf Out") +
  ylab("Species") + geom_errorbarh(aes(xmin=Budburst-sd, xmax=Budburst+sd, col="coral"), height=.0) + 
  geom_errorbarh(aes(xmin=Leaves-sd.leaf, xmax=Leaves+sd.leaf, col="forestgreen"), height=.0)
plot(ts.timeline14)

y14$code <- reorder(y14$species, y14$DOY)

ts14<-ggplot((y14), aes(x=code, y=DOY)) + geom_point(aes(col=Phenophase)) + 
  xlab("Day of Year") + ylab("Species") + geom_errorbar(aes(ymin=DOY-sd, ymax=DOY+sd, col=Phenophase), width=.0)
plot(ts14)
