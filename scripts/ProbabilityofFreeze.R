# 7 April 2017 - Cat
## Make a new script based off NA_west, Eur50, etc.
# Instead of using GDDs as guidance, attempt to find probability of low 
# temperatures within a timeframe where budburst is likely for a region
# Break into 2 week periods and then make boxplots
## Help from Ben Cook!

# Currently by Week, maybe convert to monthly instead?


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
nc<-read.csv("input/N.Carolina.csv", header=TRUE)
new<-read.csv("input/Newport.csv", header=TRUE)

## Plots by week

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
lat2$week<-strftime(lat2$date,format="%W")
lat2$week<- as.numeric(as.character(lat2$week)) - 12
lat2$Tmean <- (lat2$Tmax + lat2$Tmin)/2
lat2$gdd <- lat2$Tmean - 5
lat2$gdd <-ifelse(lat2$gdd>0, lat2$gdd, 0)
lat2$frz<- ifelse((lat2$Tmin<=-2.2), 1, 0)
lat2$count <- ave(
  lat2$frz, lat2$week, lat2$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat2<- lat2 %>%
  filter(doy >= 90) %>%
  filter(doy <= 120)

gm.count<- lat2 %>%
  dplyr::select(year, week, count) %>%
  group_by(year, week)%>%
  filter(row_number(week)==n())
gm.count$mean<-ave(gm.count$count, gm.count$week)
gm.count$stand_dev<-ave(gm.count$count, gm.count$week, FUN=sd)
gm<-gm.count %>%
  ungroup(gm.count) %>%
  dplyr::select(-count, -year) 
gm<-gm%>% filter (! duplicated(week))
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
am8<- am8 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
am8$doy<-yday(am8$date)
am8$year<-substr(am8$date,0,4)
am8$month<-substr(am8$date, 6,7)
am8$week<-strftime(am8$date,format="%W")
am8$week<- as.numeric(as.character(am8$week)) - 10
am8$Tmean <- (am8$Tmax + am8$Tmin)/2
am8$gdd <- am8$Tmean - 5
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, 0)
am8$frz<- ifelse((am8$Tmin<=-2.2), 1, 0)
am8$count <- ave(
  am8$frz, am8$week, am8$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am8<- am8 %>%
  filter(doy >= 80) %>%
  filter(doy <= 120)

wash.count<- am8 %>%
  dplyr::select(year, week, count) %>%
  group_by(year, week)%>%
  filter(row_number(week)==n())
wash.count$mean<-ave(wash.count$count, wash.count$week)
wash.count$stand_dev<-ave(wash.count$count, wash.count$week, FUN=sd)
wash<-wash.count %>%
  ungroup(wash.count) %>%
  dplyr::select(-count, -year) 
wash<-wash%>% filter (! duplicated(week))
wash$site<-"Washington"
wash.count$site<-"Washington"

# North Carolina: Feb 21 - Apr 4
## 1967 and 2012 both have one extra count (-9999degC)
nc1<-nc %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "REIDSVILLE 2 NW NC US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
nc1$year <- substr(nc1$date, 0, 4)
nc1<- nc1 %>%
  filter(year>=1965) %>%
  filter(year<2016)
nc1$month<- substr(nc1$date, 5, 6)
nc1$day<- substr(nc1$date, 7,8)
nc1<- nc1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
nc1$doy<-yday(nc1$date)
nc1$year<-substr(nc1$date,0,4)
nc1$month<-substr(nc1$date, 6,7)
nc1$week<-strftime(nc1$date,format="%W")
nc1$week<- as.numeric(as.character(nc1$week)) - 6
nc1$Tmean <- (nc1$Tmax + nc1$Tmin)/2
nc1$gdd <- nc1$Tmean - 5
nc1$gdd <-ifelse(nc1$gdd>0, nc1$gdd, 0)
nc1$frz<- ifelse((nc1$Tmin<=-2.2), 1, 0)
nc1$count <- ave(
  nc1$frz, nc1$week, nc1$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
nc1<- nc1 %>%
  filter(doy >= 50) %>%
  filter(doy <= 100)
nc1$Tmin<-ifelse(nc1$Tmin==-9999, NA, nc1$Tmin)
nc1<-na.omit(nc1)

nc.count<- nc1 %>%
  dplyr::select(year, week, count) %>%
  group_by(year, week)%>%
  filter(row_number(week)==n())
nc.count$mean<-ave(nc.count$count, nc.count$week)
nc.count$stand_dev<-ave(nc.count$count, nc.count$week, FUN=sd)
nc2<-nc.count %>%
  ungroup(nc.count) %>%
  dplyr::select(-count, -year) 
nc2<-nc2%>% filter (! duplicated(week))
nc2$site<-"North Carolina"
nc.count$site<-"North Carolina"


# Rennes, France: Apr 5 - May 10
ren<-new %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "RENNES FR") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
ren$year <- substr(ren$date, 0, 4)
ren<- ren %>% 
  filter(year>=1965) %>%
  filter(year<2016)
ren$month<- substr(ren$date, 5, 6)
ren$day<- substr(ren$date, 7,8)
ren<- ren %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
ren$doy<-yday(ren$date)
ren$year<-substr(ren$date,0,4)
ren$month<-substr(ren$date, 6,7)
ren$week<-strftime(ren$date,format="%W")
ren$week<- as.numeric(as.character(ren$week)) - 13
ren$Tmean <- (ren$Tmax + ren$Tmin)/2
ren$gdd <- ren$Tmean - 5
ren$gdd <-ifelse(ren$gdd>0, ren$gdd, 0)
ren$frz<- ifelse((ren$Tmin<=-2.2), 1, 0)
ren$count <- ave(
  ren$frz, ren$week, ren$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ren<- ren %>%
  filter(doy >= 95) %>%
  filter(doy <= 130)

ren.count<- ren %>%
  dplyr::select(year,week, count) %>%
  group_by(year, week)%>%
  filter(row_number(week)==n())
ren.count$mean<-ave(ren.count$count, ren.count$week)
ren.count$stand_dev<-ave(ren.count$count, ren.count$week, FUN=sd)
ren1<-ren.count %>%
  ungroup(ren.count) %>%
  dplyr::select(-count, -year) 
ren1<-ren1%>% filter (! duplicated(week))
ren1$site<-"France"
ren.count$site<-"France"


d<-full_join(gm, wash)
d<-full_join(d, nc2)
d<-full_join(d, ren1)
df<-full_join(gm.count,wash.count)
df<-full_join(df, nc.count)
df<-full_join(df, ren.count)

limits <- aes(ymax = mean + stand_dev, ymin=mean - stand_dev)

ggplot((d), aes(x=week, y=mean, col=site)) + geom_point() +
  geom_pointrange(limits) + scale_x_continuous(breaks=0:9)

qplot(site, count, data = df, 
      geom = "boxplot", color=as.character(week)) + 
  xlab("Site")+ylab("Mean number of freeze days")

#####################################################################
## Plots by Month
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
lat2$frz<- ifelse((lat2$Tmin<=-2.2), 1, 0)
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
am8$frz<- ifelse((am8$Tmin<=-2.2), 1, 0)
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

# North Carolina: Feb 21 - Apr 4
## 1967 and 2012 both have one extra count (-9999degC)
nc1<-nc %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "REIDSVILLE 2 NW NC US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
nc1$year <- substr(nc1$date, 0, 4)
nc1<- nc1 %>%
  filter(year>=1965) %>%
  filter(year<2016)
nc1$month<- substr(nc1$date, 5, 6)
nc1$day<- substr(nc1$date, 7,8)
nc1<- nc1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
nc1$doy<-yday(nc1$date)
nc1$year<-substr(nc1$date,0,4)
nc1$month<-substr(nc1$date, 6,7)
nc1$Tmean <- (nc1$Tmax + nc1$Tmin)/2
nc1$gdd <- nc1$Tmean - 5
nc1$gdd <-ifelse(nc1$gdd>0, nc1$gdd, 0)
nc1$frz<- ifelse((nc1$Tmin<=-2.2), 1, 0)
nc1$count <- ave(
  nc1$frz, nc1$month, nc1$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
nc1<- nc1 %>%
  filter(doy >= 50) %>%
  filter(doy <= 100)
nc1$Tmin<-ifelse(nc1$Tmin==-9999, NA, nc1$Tmin)
nc1<-na.omit(nc1)

nc.count<- nc1 %>%
  dplyr::select(year, month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
nc.count$mean<-ave(nc.count$count, nc.count$month)
nc.count$stand_dev<-ave(nc.count$count, nc.count$month, FUN=sd)
nc2<-nc.count %>%
  ungroup(nc.count) %>%
  dplyr::select(-count, -year) 
nc2<-nc2%>% filter (! duplicated(month))
nc2$site<-"North Carolina"
nc.count$site<-"North Carolina"


# Rennes, France: Apr 5 - May 10
ren<-new %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "RENNES FR") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
ren$year <- substr(ren$date, 0, 4)
ren<- ren %>% 
  filter(year>=1965) %>%
  filter(year<2016)
ren$month<- substr(ren$date, 5, 6)
ren$day<- substr(ren$date, 7,8)
ren<- ren %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
ren$doy<-yday(ren$date)
ren$year<-substr(ren$date,0,4)
ren$month<-substr(ren$date, 6,7)
ren$Tmean <- (ren$Tmax + ren$Tmin)/2
ren$gdd <- ren$Tmean - 5
ren$gdd <-ifelse(ren$gdd>0, ren$gdd, 0)
ren$frz<- ifelse((ren$Tmin<=-2.2), 1, 0)
ren$count <- ave(
  ren$frz, ren$month, ren$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ren<- ren %>%
  filter(doy >= 95) %>%
  filter(doy <= 130)

ren.count<- ren %>%
  dplyr::select(year,month, count) %>%
  group_by(year, month)%>%
  filter(row_number(month)==n())
ren.count$mean<-ave(ren.count$count, ren.count$month)
ren.count$stand_dev<-ave(ren.count$count, ren.count$month, FUN=sd)
ren1<-ren.count %>%
  ungroup(ren.count) %>%
  dplyr::select(-count, -year) 
ren1<-ren1%>% filter (! duplicated(month))
ren1$site<-"France"
ren.count$site<-"France"


d<-full_join(gm, wash)
d<-full_join(d, nc2)
d<-full_join(d, ren1)
df<-full_join(gm.count,wash.count)
df<-full_join(df, nc.count)
df<-full_join(df, ren.count)

limits <- aes(ymax = mean + stand_dev, ymin=mean - stand_dev)

ggplot((d), aes(x=month, y=mean, col=site)) + geom_point() +
  geom_pointrange(limits) 

qplot(site, count, data = df, 
      geom = "boxplot", color=month) + 
  xlab("Site")+ylab("Mean number of freeze days")

