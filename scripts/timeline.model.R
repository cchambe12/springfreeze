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
  select(Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) %>%
  filter(Latitude<= 45)%>%
  filter(Latitude>=40)

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

dat<-full_join(dat, y5) %>%
  group_by(species, Year) %>%
  arrange(species) %>%
  select(species, Year, Risk, Latitude)

## Make a big timeline plot
ggplot((df), aes(x=Budburst, y=species)) + geom_point(aes(x= df$Budburst)) + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=df$Leaves)) + theme(legend.position="none") +
  geom_point(position = position_dodge(.5)) + geom_point(aes(col=species)) + xlab("Budburst to Leaf Out") +
  ylab("Species")


## Normalize the data
qplot(Latitude, data=dat, geom="histogram") + scale_x_sqrt()

qplot(Risk, data=dat, geom="histogram") + stat_bin(bins = 20)
rc<-scale(dat$Risk, center=TRUE, scale=FALSE)
c<-dat$Risk-mean(dat$Risk)
qplot(c) + stat_bin(bins = 20)
rz<-scale(dat$Risk, center=TRUE, scale=TRUE)
z<-(dat$Risk - mean(dat$Risk))/(2*sd(dat$Risk))
qplot(z) + stat_bin(bins = 20)

qplot(Risk~species, data=dat, geom="boxplot")

qplot(Latitude, data=dat, geom="histogram")
l<- (dat$Latitude - mean(dat$Latitude))/(2*sd(dat$Latitude))
qplot(l)
summary(dat$Latitude)

## Integration of glm and rstanarm
fit1<-glm(Year~Risk + species, data=dat)
summary(fit1)
plot(fit1)

fit<-stan_glm(Year~Risk + species, data=dat)
plot(fit, ci_level = 0.5)
plot(fit, ci_level = 0.5, pars = "beta")