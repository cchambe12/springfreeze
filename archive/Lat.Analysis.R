# 9 March 2017
# Cat
# Attempt to analyze the latitudinal gradient data
# See if there is a relationship with latitude and false springs over time
# Is there a shift with climate change?

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
library(rstanarm)

# Set Working Directory
setwd("~/Documents/git/springfreeze/input")
df<-read.csv("all.lat.csv", header=TRUE)
mat<-read.csv("matrix.lat.csv",header=TRUE, check.names=FALSE)

#Clean matrix dataframe and join dataframes
mat<-mat %>%
  gather(year, fs, -Station)
df2<-mat%>%
  filter(mat$year!="Total")%>%
  filter(Station!="Total")
d<-full_join(df, mat, by="Station")
d<-d %>%
  filter(Station!="Total")%>%
  filter(year!="Total")
d$frost<-ifelse(d$fs==0, NA, d$fs)
d<-na.omit(d)
d<-dplyr::select(d,-hf.gdd, -fs)
fs<-as.data.frame(table(d$year))%>%
  rename(year=Var1)%>%
  rename(fs=Freq)
lat<-as.data.frame(table(d$Latitude))%>%
  rename(Latitude=Var1)%>%
  rename(false.spring=Freq) 
lat$Latitude<-as.numeric(as.character(lat$Latitude))
d1<-full_join(d,fs,by="year")
d1<-full_join(d1,lat,by="Latitude")
d1$year<-as.numeric(as.character(d1$year))
write.csv(d1, file="~/Documents/git/springfreeze/output/latitude.analysis.csv", row.names=FALSE)

# Initial Plots
ggplot(d1, aes(x=year, y=fs)) + geom_point(aes(color=Latitude))
ggplot(d1, aes(x=Latitude, y=false.spring)) + geom_point(aes(color=d1$region)) + 
  geom_smooth(method="loess")

# relationships?
mod<-lm(fs~year,data=d1)
mod1<-lm(lat~Latitude + region, data=d1)

d$continent<-ifelse(d$region=="europe",0,1)

mod2<-lmer(fs~year+(1|Station), data=d)
mod3<-lmer(fs~Latitude+Longitude+(1|year), data=d)


