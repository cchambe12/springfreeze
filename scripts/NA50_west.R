## 9 March 2017 : American Transect (West Coast)
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
setwd("~/Documents/git/springfreeze/input")
america<-read.csv("NOAA_data.csv", header=TRUE)
amer<-read.csv("NOAA_data2.csv", header=TRUE)
am<-read.csv("NOAA_data3.csv", header=TRUE)
# Anthony, KS, USA: 37.15611N -98.01667
am1<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BARRAGE TEMISCAMINGUE CA") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am1$year <- substr(am1$date, 0, 4)
am1<- am1 %>%
  filter(year>=1975) %>%
  filter(year<2016)
am1$month<- substr(am1$date, 5, 6)
am1$day<- substr(am1$date, 7,8)
am1<- am1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
am1$doy<-yday(am1$date)
am1$year<-substr(am1$date,0,4)
am1$Tmean <- (am1$Tmax + am1$Tmin)/2
am1$gdd <- am1$Tmean - 5
am1$gdd <-ifelse(am1$gdd>0, am1$gdd, 0)
am1$frz<- ifelse((am1$Tmin<=-5), "freeze", "thaw")
am1$count <- ave(
  am1$gdd, am1$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am1<- am1 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)

am1$fs<- ifelse((am1$count >= 150 & am1$frz == "freeze" & am1$count<=400), TRUE, NA)

barr.count<- dplyr::select(am1, year, fs)
barr.count<-na.omit(barr.count)
barr.count<-as.data.frame(table(barr.count$year))

# Anthony, KS, USA: 37.15611N -98.01667
am2<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BLUE RIVER A CA") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am2$year <- substr(am2$date, 0, 4)
am2<- am2 %>%
  filter(year>=1978) %>%
  filter(year<2016)
am2$month<- substr(am2$date, 5, 6)
am2$day<- substr(am2$date, 7,8)
am2<- am2 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
am2$doy<-yday(am2$date)
am2$year<-substr(am2$date,0,4)
am2$Tmean <- (am2$Tmax + am2$Tmin)/2
am2$gdd <- am2$Tmean - 5
am2$gdd <-ifelse(am2$gdd>0, am2$gdd, 0)
am2$frz<- ifelse((am2$Tmin<=-5), "freeze", "thaw")
am2$count <- ave(
  am2$gdd, am2$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am2<- am2 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am2$fs<- ifelse((am2$count >= 150 & am2$frz == "freeze" & am2$count<=400), TRUE, NA)

blue.count<- dplyr::select(am2, year, fs)
blue.count<-na.omit(blue.count)
blue.count<-as.data.frame(table(blue.count$year))

# West Point, NE, USA: 41.85N -96.71667E
am3<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HAY RIVER A CA") %>%
  rename(Tmean=TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am3$year <- substr(am3$date, 0, 4)
am3<- am3 %>%
  filter(year>=1978) %>%
  filter(year<2016)
am3$month<- substr(am3$date, 5, 6)
am3$day<- substr(am3$date, 7,8)
am3<- am3 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmean, Tmin, Tmax)
am3$doy<-yday(am3$date)
am3$year<-substr(am3$date,0,4)
am3$gdd <- am3$Tmean - 5
am3$gdd <-ifelse(am3$gdd>0, am3$gdd, 0)
am3$frz<- ifelse((am3$Tmean<=-2), "freeze", "thaw")
am3$count <- ave(
  am3$gdd, am3$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am3<- am3 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am3$fs<- ifelse((am3$count >= 150 & am3$frz == "freeze" & am3$count<=400), TRUE, NA)

hay.count<- dplyr::select(am3, year, fs)
hay.count<-na.omit(hay.count)
hay.count<-as.data.frame(table(hay.count$year))

# Brookings NE, USA: 44.31667N -96.76667E
am4<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "THOMPSON A CA") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am4$year <- substr(am4$date, 0, 4)
am4<- am4 %>% 
  filter(year>=1993) %>%
  filter(year<2016)
am4$month<- substr(am4$date, 5, 6)
am4$day<- substr(am4$date, 7,8)
am4<- am4 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean,Tmin, Tmax)
am4$doy<-yday(am4$date)
am4$year<-substr(am4$date,0,4)
am4$gdd <- am4$Tmean - 5
am4$gdd <-ifelse(am4$gdd>0, am4$gdd, 0)
am4$frz<- ifelse((am4$Tmean<=-2), "freeze", "thaw")
am4$count <- ave(
  am4$gdd, am4$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am4<- am4 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am4$fs<- ifelse((am4$count >= 150 & am4$frz == "freeze" & am4$count<=400), TRUE, NA)

thom.count<- dplyr::select(am4, year, fs)
thom.count<-na.omit(thom.count)
thom.count<-as.data.frame(table(thom.count$year))

# Aberdeen, SD, USA: 45.45N -98.4333E
am5<-amer %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "DALLESPORT AIRPORT WA US") %>%
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
  dplyr::select(date,Tmin, Tmax)
am5$doy<-yday(am5$date)
am5$year<-substr(am5$date,0,4)
am5$Tmean <- (am5$Tmax + am5$Tmin)/2
am5$gdd <- am5$Tmean - 5
am5$gdd <-ifelse(am5$gdd>0, am5$gdd, 0)
am5$frz<- ifelse((am5$Tmin<=-5), "freeze", "thaw")
am5$count <- ave(
  am5$gdd, am5$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am5<- am5 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am5$fs<- ifelse((am5$count >= 150 & am5$frz == "freeze" & am5$count<=400), TRUE, NA)

dal.count<- dplyr::select(am5, year, fs)
dal.count<-na.omit(dal.count)
dal.count<-as.data.frame(table(dal.count$year))

# Pembina, ND, USA: 48.96667N -97.2333E
am6<-amer %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "SPOKANE INTERNATIONAL AIRPORT WA US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am6$year <- substr(am6$date, 0, 4)
am6<- am6 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am6$month<- substr(am6$date, 5, 6)
am6$day<- substr(am6$date, 7,8)
am6<- am6 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
am6$doy<-yday(am6$date)
am6$year<-substr(am6$date,0,4)
am6$Tmean <- (am6$Tmax + am6$Tmin)/2
am6$gdd <- am6$Tmean - 5
am6$gdd <-ifelse(am6$gdd>0, am6$gdd, 0)
am6$frz<- ifelse((am6$Tmin<=-5), "freeze", "thaw")
am6$count <- ave(
  am6$gdd, am6$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am6<- am6 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am6$fs<- ifelse((am6$count >= 150 & am6$frz == "freeze" & am6$count<=400), TRUE, NA)

spo.count<- dplyr::select(am6, year, fs)
spo.count<-na.omit(spo.count)
spo.count<-as.data.frame(table(spo.count$year))

# Pembina, ND, USA: 48.96667N -97.2333E
am7<-amer %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "WENATCHEE PANGBORN AIRPORT WA US") %>%
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
  dplyr::select(date,Tmin, Tmax)
am7$doy<-yday(am7$date)
am7$year<-substr(am7$date,0,4)
am7$Tmean<- (am7$Tmax + am7$Tmin)/2
am7$gdd <- am7$Tmean - 5
am7$gdd <-ifelse(am7$gdd>0, am7$gdd, 0)
am7$frz<- ifelse((am7$Tmin<=-5), "freeze", "thaw")
am7$count <- ave(
  am7$gdd, am7$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am7<- am7 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am7$fs<- ifelse((am7$count >= 150 & am7$frz == "freeze" & am7$count<=400), TRUE, NA)

wena.count<- dplyr::select(am7, year, fs)
wena.count<-na.omit(wena.count)
wena.count<-as.data.frame(table(wena.count$year))

# Hastings, NE, USA: 40.58333N -98.35
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
am8<- am8 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
am8$doy<-yday(am8$date)
am8$year<-substr(am8$date,0,4)
am8$Tmean <- (am8$Tmax + am8$Tmin)/2
am8$gdd <- am8$Tmean - 5
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, 0)
am8$frz<- ifelse((am8$Tmin<=-5), "freeze", "thaw")
am8$count <- ave(
  am8$gdd, am8$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am8<- am8 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am8$fs<- ifelse((am8$count >= 150 & am8$frz == "freeze" & am8$count<=400), TRUE, NA)

yak.count<- dplyr::select(am8, year, fs)
yak.count<-na.omit(yak.count)
yak.count<-as.data.frame(table(yak.count$year))

# Yankton, SD, USA: 42.88333N -97.35
am9<-am %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "MEDFORD ROGUE VALLEY INTERNATIONAL AIRPORT OR US") %>%
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
  dplyr::select(date, Tmin, Tmax)
am9$doy<-yday(am9$date)
am9$year<-substr(am9$date,0,4)
am9$Tmean <- (am9$Tmax + am9$Tmin)/2
am9$gdd <- am9$Tmax - 5
am9$gdd <-ifelse(am9$gdd>0, am9$gdd, 0)
am9$frz<- ifelse((am9$Tmin<=-5), "freeze", "thaw")
am9$count <- ave(
  am9$gdd, am9$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am9<- am9 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am9$fs<- ifelse((am9$count >= 150 & am9$frz == "freeze" & am9$count<=400), TRUE, NA)

med.count<- dplyr::select(am9, year, fs)
med.count<-na.omit(med.count)
med.count<-as.data.frame(table(med.count$year))
