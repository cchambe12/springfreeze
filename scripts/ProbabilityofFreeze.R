# 7 April 2017 - Cat
## Make a new script based off NA_west, Eur50, etc.
# Instead of using GDDs as guidance, attempt to find probability of low 
# temperatures within a timeframe where budburst is likely for a region
# Break into 2 week periods and then make boxplots
## Help from Ben Cook!


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
lat<-read.csv("input/NOAA_Eur50.csv", header=TRUE)
amer<-read.csv("input/NOAA_data2.csv", header=TRUE)


# Bamberg,Germany: DOY 90-120
lat2<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BAMBERG GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat2$year <- substr(lat2$date, 0, 4)
lat2<- lat2 %>%
  filter(year>=1965) %>%
  filter(year<2016)
lat2$month<- substr(lat2$date, 5, 6)
lat2$day<- substr(lat2$date, 7,8)
lat2<- lat2 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat2$doy<-yday(lat2$date)
lat2$year<-substr(lat2$date,0,4)
lat2$month<-substr(lat2$date, 6,7)
lat2$Tmean <- (lat2$Tmax + lat2$Tmin)/2
lat2$gdd <- lat2$Tmean - 5
lat2$gdd <-ifelse(lat2$gdd>0, lat2$gdd, 0)
lat2$frz<- ifelse((lat2$Tmin<=-5), 1, 0)
lat2$count <- ave(
  lat2$frz, lat2$month, lat2$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat2<- lat2 %>%
  filter(doy >= 90) %>%
  filter(doy <= 120)

gm.count<- lat2 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
gm.count$mean<-ave(gm.count$count, gm.count$month)
gm.count$stand_dev<-ave(gm.count$count, gm.count$month, FUN=sd)
gm<-gm.count %>%
  ungroup(gm.count) %>%
  dplyr::select(-count, -year) 
gm<-gm%>% filter (! duplicated(month))
gm$site<-"Germany"
gm.count$site<-"Germany"

# Yakima Airport: March 22-Apr 30
am8<-amer %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "YAKIMA AIRPORT WA US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am8$year <- substr(am8$date, 0, 4)
am8<- am8 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am8$month<- substr(am8$date, 5, 6)
am8$day<- substr(am8$date, 7,8)
am8$weeknum <- as.numeric(format(am8$date, "%U"))
am8<- am8 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
am8$doy<-yday(am8$date)
am8$year<-substr(am8$date,0,4)
am8$month<-substr(am8$date, 6,7)
am8$Tmean <- (am8$Tmax + am8$Tmin)/2
am8$gdd <- am8$Tmean - 5
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, 0)
am8$frz<- ifelse((am8$Tmin<=-5), 1, 0)
am8$count <- ave(
  am8$frz, am8$month, am8$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am8<- am8 %>%
  filter(doy >= 80) %>%
  filter(doy <= 120)
  

wash.count<- am8 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
wash.count$mean<-ave(wash.count$count, wash.count$month)
wash.count$stand_dev<-ave(wash.count$count, wash.count$month, FUN=sd)
wash<-wash.count %>%
  ungroup(wash.count) %>%
  dplyr::select(-count, -year) 
wash<-wash%>% filter (! duplicated(month))
wash$site<-"Washington"
wash.count$site<-"Washington"

d<-full_join(gm, wash)
df<-full_join(gm.count,wash.count)

limits <- aes(ymax = mean + stand_dev, ymin=mean - stand_dev)

ggplot((d), aes(x=month, y=mean, col=site)) + geom_point() +
  geom_pointrange(limits)

qplot(site, count, data = df, 
      geom = "boxplot", color=month) + 
  xlab("Site")+ylab("Mean number of freeze days")
