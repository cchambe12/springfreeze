## 30 November 2016
# Weather Data Script - Cat
# Aim: To create a table that indicates prevalence of FSI events by latitude
# by simply analyzing weather data
# Starting with our sites, hoping to apply to site further north and south

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
weather<-read.csv("input/WeatherData.csv",header=TRUE)

# Subset Weather down for HF
weather<- weather %>%
  filter(Site == "hf") %>%
  filter(Year > 2000) %>%
  filter(JD > 60) %>%
  filter(JD < 181)

for(i in unique(budburst$year)){ # i = '2000'
  dxx <- budburst[budburst$year == i,]
  lox <- rank(dxx$l75.jd)
  lox[is.na(dxx$l75.jd)] = NA
  box <- rank(dxx$bb.jd, na.last = TRUE)
  box[is.na(dxx$bb.jd)] = NA
  lo = c(lo, lox)
  bb = c(bb, box)
  sp = c(sp, dxx$sp)
  yr = c(yr, rep(i, nrow(dxx)))
}

# rowSums(data > 0)
# apply(data>30,7,identity)

for(i in unique(weather$AirT)){
  apply(weather$AirT>0, 7, identity)
}
