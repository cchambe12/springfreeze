## 8 March 2017
# GDD Script and Budburst for HF
# Aim: To find the average accumulated growing degree days for Harvard Forest till budburst
# using John O'Keefe's data

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
hf<- read.csv("input/WeatherData.csv", header=TRUE)
d<-read.csv("input/method.test.csv",header=TRUE)

# Harvard Forest
# To double check my script is accurate
hf<- filter(hf, Site == "hf")
hf$gdd <- hf$AirT - 5
hf$gdd <-ifelse(hf$gdd>0, hf$gdd, 0)
hf$count <- ave(
  hf$gdd, hf$Year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

# Clean up dataframes and join
hf<-rename(hf, year=Year)
d<-d%>%
  dplyr::select(year, bb_obs)
d<- as.data.frame(rapply(object = d, f = round, classes = "numeric", how = "replace", digits = 0)) 
df<-left_join(hf, d)
df<-df%>%
  dplyr::select(year, JD, bb_obs, count) %>%
  filter(year>=1990)
df$doy<-ifelse(df$JD==df$bb_obs, df$JD, NA)
df<-na.omit(df)
mean<-mean(df$count)