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
mid<-read.csv("input/midwest.csv", header=TRUE)

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
lat2$week<- as.numeric(as.character(lat2$week)) - 11

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

# Waterville, ME: DOY 100-150
water<-mid %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "WATERVILLE WWTP ME US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
water$year <- substr(water$date, 0, 4)
water<- water %>%
  filter(year>=1965) %>%
  filter(year<2016)
water$month<- substr(water$date, 5, 6)
water$day<- substr(water$date, 7,8)
water<- water %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
water$doy<-yday(water$date)
water$year<-substr(water$date,0,4)
water$month<-substr(water$date, 6,7)
water$week<-strftime(water$date,format="%W")
water$Tmean <- (water$Tmax + water$Tmin)/2
water$gdd <- water$Tmean - 5
water$gdd <-ifelse(water$gdd>0, water$gdd, 0)
water$frz<- ifelse((water$Tmin<=-2.2), 1, 0)
water$count <- ave(
  water$frz, water$week, water$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
water<- water %>%
  filter(doy >= 100) %>%
  filter(doy <= 150)
water$week<- as.numeric(as.character(water$week)) - 13
water$Tmin<-ifelse(water$Tmin==-9999, NA, water$Tmin)
water<-na.omit(water)


water.count<- water %>%
  dplyr::select(year, week, count) %>%
  group_by(year, week)%>%
  filter(row_number(week)==n())
water.count$mean<-ave(water.count$count, water.count$week)
water.count$stand_dev<-ave(water.count$count, water.count$week, FUN=sd)
maine<-water.count %>%
  ungroup(water.count) %>%
  dplyr::select(-count, -year) 
maine<-maine%>% filter (! duplicated(week))
maine$site<-"Maine"
water.count$site<-"Maine"

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
am8$week<- as.numeric(as.character(am8$week)) - 10

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
nc1$week<- as.numeric(as.character(nc1$week)) - 6
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
ren$week<- as.numeric(as.character(ren$week)) - 12

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
d<-full_join(d, maine)
df<-full_join(gm.count,wash.count)
df<-full_join(df, nc.count)
df<-full_join(df, ren.count)
df<-full_join(df, water.count)

limits <- aes(ymax = mean + stand_dev, ymin=mean - stand_dev)

ggplot((d), aes(x=week, y=mean, col=site)) + geom_point() +
  geom_pointrange(limits) + scale_x_continuous(breaks=0:9)

qplot(site, count, data = df, 
      geom = "boxplot", color=as.character(week)) + 
  xlab("Site")+ylab("Mean number of freeze days")

#####################################################################
## Plots by Month
# Bamberg,Germany: DOY 90-120
lat1<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BAMBERG GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat1$year <- substr(lat1$date, 0, 4)
lat1<- lat1 %>%
  filter(year>=1965) %>%
  filter(year<2016)
lat1$month<- substr(lat1$date, 5, 6)
lat1$day<- substr(lat1$date, 7,8)
lat1<- lat1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat1$doy<-yday(lat1$date)
lat1$day<-substr(lat1$date,9,10)
lat1$year<-substr(lat1$date,0,4)
lat1$month<-substr(lat1$date, 6,7)
lat1$biweek<-ifelse(lat1$day<=15,1,2)
lat1<-lat1%>%unite(biweekly,month,biweek, sep="_")
lat1$Tmean <- (lat1$Tmax + lat1$Tmin)/2
lat1$gdd <- lat1$Tmean - 5
lat1$gdd <-ifelse(lat1$gdd>0, lat1$gdd, 0)
lat1$frz<- ifelse((lat1$Tmin<=-2.2), 1, 0)
lat1$count <- ave(
  lat1$frz, lat1$biweekly, lat1$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat1<- lat1 %>%
  filter(doy >= 90) %>%
  filter(doy <= 120)

gm.count<- lat1 %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
gm.count$mean<-ave(gm.count$count, gm.count$biweekly)
gm.count$stand_dev<-ave(gm.count$count, gm.count$biweekly, FUN=sd)
gm<-gm.count %>%
  ungroup(gm.count) %>%
  dplyr::select(-count, -year) 
gm<-gm%>% filter (! duplicated(biweekly))
gm$site<-"Germany"
gm.count$site<-"Germany"

# Waterville, ME: DOY 100-150
water<-mid %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "WATERVILLE WWTP ME US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
water$year <- substr(water$date, 0, 4)
water<- water %>%
  filter(year>=1965) %>%
  filter(year<2016)
water$month<- substr(water$date, 5, 6)
water$day<- substr(water$date, 7,8)
water<- water %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
water$doy<-yday(water$date)
water$year<-substr(water$date,0,4)
water$month<-substr(water$date, 6,7)
water$day<-substr(water$date, 9,10)
water$biweek<-ifelse(water$day<=15,1,2)
water<-water%>%unite(biweekly,month,biweek, sep="_")
water$Tmean <- (water$Tmax + water$Tmin)/2
water$gdd <- water$Tmean - 5
water$gdd <-ifelse(water$gdd>0, water$gdd, 0)
water$frz<- ifelse((water$Tmin<=-2.2), 1, 0)
water$count <- ave(
  water$frz, water$biweekly, water$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
water<- water %>%
  filter(doy >= 100) %>%
  filter(doy <= 150)

water.count<- water %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
water.count$mean<-ave(water.count$count, water.count$biweekly)
water.count$stand_dev<-ave(water.count$count, water.count$biweekly, FUN=sd)
maine<-water.count %>%
  ungroup(water.count) %>%
  dplyr::select(-count, -year) 
maine<-maine%>% filter (! duplicated(biweekly))
maine$site<-"Maine"
water.count$site<-"Maine"

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
am8$day<-substr(am8$date, 9,10)
am8$biweek<-ifelse(am8$day<=15,1,2)
am8<-am8%>%unite(biweekly,month,biweek, sep="_")
am8$Tmean <- (am8$Tmax + am8$Tmin)/2
am8$gdd <- am8$Tmean - 5
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, 0)
am8$frz<- ifelse((am8$Tmin<=-2.2), 1, 0)
am8$count <- ave(
  am8$frz, am8$biweekly, am8$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am8<- am8 %>%
  filter(doy >= 80) %>%
  filter(doy <= 120)

wash.count<- am8 %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
wash.count$mean<-ave(wash.count$count, wash.count$biweekly)
wash.count$stand_dev<-ave(wash.count$count, wash.count$biweekly, FUN=sd)
wash<-wash.count %>%
  ungroup(wash.count) %>%
  dplyr::select(-count, -year) 
wash<-wash%>% filter (! duplicated(biweekly))
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
nc1$day<-substr(nc1$date,9,10)
nc1$biweek<-ifelse(nc1$day<=15,1,2)
nc1<-nc1%>%unite(biweekly,month,biweek, sep="_")
nc1$Tmean <- (nc1$Tmax + nc1$Tmin)/2
nc1$gdd <- nc1$Tmean - 5
nc1$gdd <-ifelse(nc1$gdd>0, nc1$gdd, 0)
nc1$frz<- ifelse((nc1$Tmin<=-2.2), 1, 0)
nc1$count <- ave(
  nc1$frz, nc1$biweekly, nc1$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
nc1<- nc1 %>%
  filter(doy >= 50) %>%
  filter(doy <= 100)
nc1$Tmin<-ifelse(nc1$Tmin==-9999, NA, nc1$Tmin)
nc1<-na.omit(nc1)

nc.count<- nc1 %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
nc.count$mean<-ave(nc.count$count, nc.count$biweekly)
nc.count$stand_dev<-ave(nc.count$count, nc.count$biweekly, FUN=sd)
nc2<-nc.count %>%
  ungroup(nc.count) %>%
  dplyr::select(-count, -year) 
nc2<-nc2%>% filter (! duplicated(biweekly))
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
ren$day<-substr(ren$date,9,10)
ren$biweek<-ifelse(ren$day<=15,1,2)
ren<-ren%>%unite(biweekly,month,biweek, sep="_")
ren$Tmean <- (ren$Tmax + ren$Tmin)/2
ren$gdd <- ren$Tmean - 5
ren$gdd <-ifelse(ren$gdd>0, ren$gdd, 0)
ren$frz<- ifelse((ren$Tmin<=-2.2), 1, 0)
ren$count <- ave(
  ren$frz, ren$biweekly, ren$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ren<- ren %>%
  filter(doy >= 95) %>%
  filter(doy <= 130)

ren.count<- ren %>%
  dplyr::select(year,biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
ren.count$mean<-ave(ren.count$count, ren.count$biweekly)
ren.count$stand_dev<-ave(ren.count$count, ren.count$biweekly, FUN=sd)
ren1<-ren.count %>%
  ungroup(ren.count) %>%
  dplyr::select(-count, -year) 
ren1<-ren1%>% filter (! duplicated(biweekly))
ren1$site<-"France"
ren.count$site<-"France"


d<-full_join(gm, wash)
d<-full_join(d, nc2)
d<-full_join(d, ren1)
d<-full_join(d, maine)
df<-full_join(gm.count,wash.count)
df<-full_join(df, nc.count)
df<-full_join(df, ren.count)
df<-full_join(df, water.count)



limits <- aes(ymax = mean + stand_dev, ymin=mean - stand_dev)

ggplot((d), aes(x=biweekly, y=mean, col=site)) + geom_point() +
  geom_pointrange(limits) 

df$biweekly2<-ifelse(df$biweekly=="02_2", "Feb 15 - Feb 29", df$biweekly2)
df$biweekly2<-ifelse(df$biweekly=="03_1", "Mar 1 - Mar 14", df$biweekly2)
df$biweekly2<-ifelse(df$biweekly=="03_2", "Mar 15 - Mar 31", df$biweekly2)
df$biweekly2<-ifelse(df$biweekly=="04_1", "Apr 1 - Apr 14", df$biweekly2)
df$biweekly2<-ifelse(df$biweekly=="04_2", "Apr 15 - Apr 30", df$biweekly2)
df$biweekly2<-ifelse(df$biweekly=="05_1", "May 1 - May 14", df$biweekly2)
df$biweekly2<-ifelse(df$biweekly=="05_2", "May 15 - May 31", df$biweekly2)

df_table <- table(df$biweekly)
df_levels <- names(df_table)[order(df_table)]
df$biweekly3 <- factor(df$biweekly2, levels = df_levels)
qplot(site, count, data = df, 
      geom = "boxplot", color=biweekly) + 
  xlab("Site")+ylab("Mean number of freeze days")


