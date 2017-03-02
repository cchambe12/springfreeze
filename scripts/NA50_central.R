## 2 March 2017 : American Transect (Kansas to North Dakota)
# Weather Data Script (based off of Weather_Latitude.R) - Cat
# Aim: To create a table that indicates prevalence of FSI events by latitude
# by simply analyzing weather data
# Starting with our sites, hoping to apply to site further north and south
# Looking to change my calculation for GDD from original script and use more data!

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
setwd("~/Desktop")
america<-read.csv("NOAA_Am50.csv", header=TRUE)
# Anthony, KS, USA: 37.15611N -98.01667
am1<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ANTHONY KS US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am1$year <- substr(am1$date, 0, 4)
am1<- am1 %>%
  filter(year>=1965) %>%
  filter(year<2016)
am1$month<- substr(am1$date, 5, 6)
am1$day<- substr(am1$date, 7,8)
am1<- am1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
am1$doy<-yday(am1$date)
am1$year<-substr(am1$date,0,4)
am1<- am1 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am1$gdd <- am1$Tmax - 5
am1$gdd <-ifelse(am1$gdd>0, am1$gdd, 0)
am1$frz<- ifelse((am1$Tmin<=-3), "freeze", "thaw")
am1$count <- ave(
  am1$gdd, am1$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am1$fs<- ifelse((am1$count >= 250 & am1$frz == "freeze"), TRUE, NA)

anth.count<- dplyr::select(am1, year, fs)
anth.count<-na.omit(anth.count)
anth.count<-as.data.frame(table(anth.count$year))

# West Point, NE, USA: 41.85N -96.71667E
am3<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "WEST POINT NE US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am3$year <- substr(am3$date, 0, 4)
am3<- am3 %>%
  filter(year>=1965) %>%
  filter(year<2016)
am3$month<- substr(am3$date, 5, 6)
am3$day<- substr(am3$date, 7,8)
am3<- am3 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
am3$doy<-yday(am3$date)
am3$year<-substr(am3$date,0,4)
am3<- am3 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am3$gdd <- am3$Tmax - 5
am3$gdd <-ifelse(am3$gdd>0, am3$gdd, 0)
am3$frz<- ifelse((am3$Tmin<=-3), "freeze", "thaw")
am3$count <- ave(
  am3$gdd, am3$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am3$fs<- ifelse((am3$count >= 250 & am3$frz == "freeze"), TRUE, NA)

west.count<- dplyr::select(am3, year, fs)
west.count<-na.omit(west.count)
west.count<-as.data.frame(table(west.count$year))

# Brookings NE, USA: 44.31667N -96.76667E
am4<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BROOKINGS 2 NE SD US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am4$year <- substr(am4$date, 0, 4)
am4<- am4 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am4$month<- substr(am4$date, 5, 6)
am4$day<- substr(am4$date, 7,8)
am4<- am4 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
am4$doy<-yday(am4$date)
am4$year<-substr(am4$date,0,4)
am4<- am4 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am4$gdd <- am4$Tmax - 5
am4$gdd <-ifelse(am4$gdd>0, am4$gdd, 0)
am4$frz<- ifelse((am4$Tmin<=-3), "freeze", "thaw")
am4$count <- ave(
  am4$gdd, am4$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am4$fs<- ifelse((am4$count >= 250 & am4$frz == "freeze"), TRUE, NA)

bro.count<- dplyr::select(am4, year, fs)
bro.count<-na.omit(bro.count)
bro.count<-as.data.frame(table(bro.count$year))

# Aberdeen, SD, USA: 45.45N -98.4333E
am5<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ABERDEEN REGIONAL AIRPORT SD US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am5$year <- substr(am5$date, 0, 4)
am5<- am5 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am5$month<- substr(am5$date, 5, 6)
am5$day<- substr(am5$date, 7,8)
am5<- am5 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
am5$doy<-yday(am5$date)
am5$year<-substr(am5$date,0,4)
am5<- am5 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am5$gdd <- am5$Tmax - 5
am5$gdd <-ifelse(am5$gdd>0, am5$gdd, 0)
am5$frz<- ifelse((am5$Tmin<=-3), "freeze", "thaw")
am5$count <- ave(
  am5$gdd, am5$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am5$fs<- ifelse((am5$count >= 250 & am5$frz == "freeze"), TRUE, NA)

abe.count<- dplyr::select(am5, year, fs)
abe.count<-na.omit(abe.count)
abe.count<-as.data.frame(table(abe.count$year))

# Pembina, ND, USA: 48.96667N -97.2333E
am7<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "PEMBINA ND US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am7$year <- substr(am7$date, 0, 4)
am7<- am7 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am7$month<- substr(am7$date, 5, 6)
am7$day<- substr(am7$date, 7,8)
am7<- am7 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
am7$doy<-yday(am7$date)
am7$year<-substr(am7$date,0,4)
am7<- am7 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am7$gdd <- am7$Tmax - 5
am7$gdd <-ifelse(am7$gdd>0, am7$gdd, 0)
am7$frz<- ifelse((am7$Tmin<=-3), "freeze", "thaw")
am7$count <- ave(
  am7$gdd, am7$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am7$fs<- ifelse((am7$count >= 250 & am7$frz == "freeze"), TRUE, NA)

pem.count<- dplyr::select(am7, year, fs)
pem.count<-na.omit(pem.count)
pem.count<-as.data.frame(table(pem.count$year))

# Hastings, NE, USA: 40.58333N -98.35
am8<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HASTINGS 4 N NE US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am8$year <- substr(am8$date, 0, 4)
am8<- am8 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am8$month<- substr(am8$date, 5, 6)
am8$day<- substr(am8$date, 7,8)
am8<- am8 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
am8$doy<-yday(am8$date)
am8$year<-substr(am8$date,0,4)
am8<- am8 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am8$gdd <- am8$Tmax - 5
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, 0)
am8$frz<- ifelse((am8$Tmin<=-3), "freeze", "thaw")
am8$count <- ave(
  am8$gdd, am8$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am8$fs<- ifelse((am8$count >= 250 & am8$frz == "freeze"), TRUE, NA)

has.count<- dplyr::select(am8, year, fs)
has.count<-na.omit(has.count)
has.count<-as.data.frame(table(has.count$year))

# Yankton, SD, USA: 42.88333N -97.35
am9<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "YANKTON SD US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am9$year <- substr(am9$date, 0, 4)
am9<- am9 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am9$month<- substr(am9$date, 5, 6)
am9$day<- substr(am9$date, 7,8)
am9<- am9 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
am9$doy<-yday(am9$date)
am9$year<-substr(am9$date,0,4)
am9<- am9 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am9$gdd <- am9$Tmax - 5
am9$gdd <-ifelse(am9$gdd>0, am9$gdd, 0)
am9$frz<- ifelse((am9$Tmin<=-3), "freeze", "thaw")
am9$count <- ave(
  am9$gdd, am9$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am9$fs<- ifelse((am9$count >= 250 & am9$frz == "freeze"), TRUE, NA)

yan.count<- dplyr::select(am9, year, fs)
yan.count<-na.omit(yan.count)
yan.count<-as.data.frame(table(yan.count$year))

# Grand Forks, ND, USA: 47.9333N -97.08333E
am10<-america %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "GRAND FORKS UNIVERSITY NWS ND US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am10$year <- substr(am10$date, 0, 4)
am10<- am10 %>% 
  filter(year>=1965)%>%
  filter(year<2016)
am10$month<- substr(am10$date, 5, 6)
am10$day<- substr(am10$date, 7,8)
am10<- am10 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
am10$doy<-yday(am10$date)
am10$year<-substr(am10$date,0,4)
am10<- am10 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
am10$gdd <- am10$Tmax - 5
am10$gdd <-ifelse(am10$gdd>0, am10$gdd, 0)
am10$frz<- ifelse((am10$Tmin<=-3), "freeze", "thaw")
am10$count <- ave(
  am10$gdd, am10$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

am10$fs<- ifelse((am10$count >= 250 & am10$frz == "freeze"), TRUE, NA)

gra.count<- dplyr::select(am10, year, fs)
gra.count<-na.omit(gra.count)
gra.count<-as.data.frame(table(gra.count$year))