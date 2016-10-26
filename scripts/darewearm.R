## Attempt to make a bigger model using NPN data
## Question - simple
## How does the Risk time change across the years by species?
## Year ~ Risk + Species is the simple breakdown
## Try and use rstanarm! 
## 25 October 2016 : Cat

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
  select(Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year)

# Make dataframe that includes Risk for each species
y0<-pheno%>%
  filter(Year=="2010")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y0<-na.omit(y0)
y0$Risk <- y0$Leaves - y0$Budburst
y0<-filter(y0, Risk > 0)
y0<-filter(y0, Risk < 30)
y0<- y0 %>%
  group_by(species) %>%
  summarise_each(funs(mean), Risk)

attempt<- y0 %>%
  filter(species =="Acer_rubrum")

qplot(Risk, data=y0, geom="histogram")
rc<-scale(y0$Risk, center=TRUE, scale=FALSE)
c<-y0$Risk-mean(y0$Risk)
qplot(c)
rz<-scale(y0$Risk, center=TRUE, scale=TRUE)
z<-(y0$Risk - mean(y0$Risk))/sd(y0$Risk)
qplot(z)


y1<-pheno%>%
  filter(Year=="2011")%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
y1<-na.omit(y1)
y1$Risk <- y1$Leaves - y1$Budburst
y1<-filter(y1, Risk > 0)
y1<-filter(y1, Risk < 200)

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
y2<-filter(y2, Risk < 200)

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
y3<-filter(y3, Risk < 200)

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
y4<-filter(y4, Risk < 200)

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
y5<-filter(y5, Risk < 200)

dat<-full_join(dat,y5) %>%
  group_by(species, Year) %>%
  arrange(species) %>%
  select(species, Year, Risk, Latitude)

dat$Genus <- unlist(
  lapply(strsplit(row.names(dat), "_"),
         function(x) x[[1]]))


## Integration of glm and rstanarm
fit1<-glm(Year~Risk + species, data=dat)
summary(fit1)
plot(fit1)

fit<-stan_glm(Year~Risk + species, data=dat)
plot(fit, ci_level = 0.5)
plot(fit, ci_level = 0.5, pars = "beta")
