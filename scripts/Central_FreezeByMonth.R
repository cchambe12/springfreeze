# Cat - April 7 2017
## Attempt to figure out how many days per month temp is -5 at each station studied

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(arm)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
america<-read.csv("input/NOAA_Am50.csv", header=TRUE)
# Anthony, KS, USA: 37.15611N -98.01667
am1<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ANTHONY KS US") %>%
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
  dplyr::select(date, Tmin, Tmax, TAVG)
am1$doy<-yday(am1$date)
am1$year<-substr(am1$date,0,4)
am1$month<-substr(am1$date, 6,7)
am1$Tmean <- (am1$Tmax + am1$Tmin)/2
am1$gdd <- am1$Tmean - 10
am1$gdd <-ifelse(am1$gdd>0, am1$gdd, 0)
am1$frz<- ifelse((am1$Tmin<=-5), 1, 0)
am1$count <- ave(
  am1$frz, am1$month, am1$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
#am1<- am1 %>% ## Do we just want to look at Spring months?
  #filter(doy>=60)%>%
  #filter(doy<=210)

anth.count<- am1 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())

# Analysis
anth.count$mean<-ave(anth.count$count, anth.count$month)
anth.count$stand_dev<-ave(anth.count$count, anth.count$month, FUN=sd)
anth<-anth.count %>%
  ungroup(anth.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
anth$site<-"anth"
  
mod1<-lm(count~month, data=anth.count)
display(mod1)

ggplot((anth.count), aes(x=month, y=mean)) + geom_point() +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

# West Point, NE, USA: 41.85N -96.71667E
am3<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "WEST POINT NE US") %>%
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
  dplyr::select(date, TAVG, Tmin, Tmax)
am3$doy<-yday(am3$date)
am3$year<-substr(am3$date,0,4)
am3$month<-substr(am3$date, 6,7)
am3$Tmean <- (am3$Tmax + am3$Tmin)/2
am3$gdd <- am3$Tmean - 10
am3$gdd <-ifelse(am3$gdd>0, am3$gdd, 0)
am3$frz<- ifelse((am3$Tmin<=-5), 1, 0)
am3$count <- ave(
  am3$frz, am3$month, am3$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
west.count<- am3 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
# Analysis
west.count$mean<-ave(west.count$count, west.count$month)
west.count$stand_dev<-ave(west.count$count, west.count$month, FUN=sd)
ggplot((west.count), aes(x=month, y=mean)) + geom_point() +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

west<-west.count %>%
  ungroup(west.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
west$site<-"west"
d<-full_join(anth, west)

# Brookings NE, USA: 44.31667N -96.76667E
am4<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BROOKINGS 2 NE SD US") %>%
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
  dplyr::select(date, TAVG, Tmin, Tmax)
am4$doy<-yday(am4$date)
am4$year<-substr(am4$date,0,4)
am4$month<-substr(am4$date, 6,7)
am4$Tmean <- (am4$Tmax + am4$Tmin)/2
am4$gdd <- am4$Tmean - 10
am4$gdd <-ifelse(am4$gdd>0, am4$gdd, 0)
am4$frz<- ifelse((am4$Tmin<=-5), 1, 0)
am4$count <- ave(
  am4$frz, am4$month, am4$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
bro.count<- am4 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
# Analysis
bro.count$mean<-ave(bro.count$count, bro.count$month)
bro.count$stand_dev<-ave(bro.count$count, bro.count$month, FUN=sd)
ggplot((bro.count), aes(x=month, y=mean)) + geom_point() +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

bro<-bro.count %>%
  ungroup(bro.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
bro$site<-"bro"
d<-full_join(d, bro)

# Aberdeen, SD, USA: 45.45N -98.4333E
am5<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ABERDEEN REGIONAL AIRPORT SD US") %>%
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
  dplyr::select(date, TAVG, Tmin, Tmax)
am5$doy<-yday(am5$date)
am5$year<-substr(am5$date,0,4)
am5$month<-substr(am5$date, 6,7)
am5$Tmean <- (am5$Tmax + am5$Tmin)/2
am5$gdd <- am5$Tmean - 10
am5$gdd <-ifelse(am5$gdd>0, am5$gdd, 0)
am5$frz<- ifelse((am5$Tmin<=-5),1,0)
am5$count <- ave(
  am5$frz, am5$month, am5$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
abe.count<- am5 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
# Analysis
abe.count$mean<-ave(abe.count$count, abe.count$month)
abe.count$stand_dev<-ave(abe.count$count, abe.count$month, FUN=sd)
ggplot((abe.count), aes(x=month, y=mean)) + geom_point() +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

abe<-abe.count %>%
  ungroup(abe.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
abe$site<-"abe"
d<-full_join(d, abe)

# Pembina, ND, USA: 48.96667N -97.2333E
am7<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "PEMBINA ND US") %>%
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
  dplyr::select(date, TAVG, Tmin, Tmax)
am7$doy<-yday(am7$date)
am7$year<-substr(am7$date,0,4)
am7$month<-substr(am7$date, 6,7)
am7$Tmean <- (am7$Tmax + am7$Tmin)/2
am7$gdd <- am7$Tmean - 10
am7$gdd <-ifelse(am7$gdd>0, am7$gdd, 0)
am7$frz<- ifelse((am7$Tmin<=-5), 1, 0)
am7$count <- ave(
  am7$frz, am7$month, am7$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
pem.count<- am7 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
# Analysis
pem.count$mean<-ave(pem.count$count, pem.count$month)
pem.count$stand_dev<-ave(pem.count$count, pem.count$month, FUN=sd)
ggplot((pem.count), aes(x=month, y=mean)) + geom_point() +
geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

pem<-pem.count %>%
  ungroup(pem.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
pem$site<-"pem"
d<-full_join(d, pem)


# Hastings, NE, USA: 40.58333N -98.35
am8<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HASTINGS 4 N NE US") %>%
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
  dplyr::select(date, TAVG, Tmin, Tmax)
am8$doy<-yday(am8$date)
am8$year<-substr(am8$date,0,4)
am8$month<-substr(am8$date, 6,7)
am8$Tmean <- (am8$Tmax + am8$Tmin)/2
am8$gdd <- am8$Tmean - 10
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, 0)
am8$frz<- ifelse((am8$Tmin<=-5), 1, 0)
am8$count <- ave(
  am8$frz, am8$month, am8$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
has.count<- am8%>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
# Analysis
has.count$mean<-ave(has.count$count, has.count$month)
has.count$stand_dev<-ave(has.count$count, has.count$month, FUN=sd)
ggplot((has.count), aes(x=month, y=mean)) + geom_point() +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

has<-has.count %>%
  ungroup(has.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
has$site<-"has"
d<-full_join(d, has)


# Yankton, SD, USA: 42.88333N -97.35
am9<-america %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "YANKTON SD US") %>%
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
  dplyr::select(date, TAVG, Tmin, Tmax)
am9$doy<-yday(am9$date)
am9$year<-substr(am9$date,0,4)
am9$month<-substr(am9$date, 6,7)
am9$Tmean <- (am9$Tmax + am9$Tmin)/2
am9$gdd <- am9$Tmax - 10
am9$gdd <-ifelse(am9$gdd>0, am9$gdd, 0)
am9$frz<- ifelse((am9$Tmin<=-5), 1, 0)
am9$count <- ave(
  am9$frz, am9$month, am9$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
yan.count<- am9 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
# Analysis
yan.count$mean<-ave(yan.count$count, yan.count$month)
yan.count$stand_dev<-ave(yan.count$count, yan.count$month, FUN=sd)
ggplot((yan.count), aes(x=month, y=mean)) + geom_point() +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

yan<-yan.count %>%
  ungroup(yan.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
yan$site<-"yan"
d<-full_join(d, yan)

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
am10$month<-substr(am10$date, 6,7)
am10$Tmean <- (am10$Tmax + am10$Tmin)/2
am10$gdd <- am10$Tmax - 10
am10$gdd <-ifelse(am10$gdd>0, am10$gdd, 0)
am10$frz<- ifelse((am10$Tmin<=-5), 1, 0)
am10$count <- ave(
  am10$frz, am10$month, am10$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
gra.count<- am10 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
# Analysis
gra.count$mean<-ave(gra.count$count, gra.count$month)
gra.count$stand_dev<-ave(gra.count$count, gra.count$month, FUN=sd)
ggplot((gra.count), aes(x=month, y=mean)) + geom_point() +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), width=.0)

gra<-gra.count %>%
  ungroup(gra.count) %>%
  dplyr::select(-count, -year) %>%
  filter (! duplicated(month))
gra$site<-"gra"
d<-full_join(d, gra)

ggplot((d), aes(x=month, y=mean)) + geom_point(aes(col=site)) +
  geom_errorbar(aes(ymin=mean-stand_dev, ymax=mean+stand_dev), aes(col=d$site), width=.0)



