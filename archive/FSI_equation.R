# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)

setwd("~/Documents/git/springfreeze/input")
hf<- read.csv("WeatherData.csv", header=TRUE)

hf<- hf %>%
  filter(Year>=2003) %>%
  dplyr::select(JD, AirT, Year)
hf$gdd<- hf$AirT - 5
hf$gdd <-ifelse(hf$gdd>0, hf$gdd, 0)
hf$count <- ave(
    hf$gdd, hf$Year, 
    FUN=function(x) cumsum(c(0, head(x, -1)))
  )

results<- hf %>%
  filter(count>=100 & count<=400 & hf$JD>=60 & hf$JD<=210
         & AirT<=-2.2)
