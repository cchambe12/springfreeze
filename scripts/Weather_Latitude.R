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
lat<-read.csv("input/germany.csv", header=TRUE)
america<- read.csv("input/northamerica.csv", header=TRUE)
italy<- read.csv("input/italy.csv", header=TRUE)
kansas<- read.csv("input/kansas.csv", header=TRUE)
hf<- read.csv("input/WeatherData.csv", header=TRUE)

# Harvard Forest
# To double check my script is accurate
hf<- filter(hf, Site == "hf")
hf$gdd <- hf$AirT - 10
hf$gdd <-ifelse(hf$gdd>0, hf$gdd, NA)
hf$warm<- ifelse(!is.na(hf$gdd), 1, 0)
hf$frz<- ifelse((hf$AirTMin<=-2), "freeze", "thaw")
hf$count <- ave(
  hf$warm, hf$Year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
hf<- hf %>%
  filter(JD >= 60) %>%
  filter(JD <= 181)
hf$fs<- ifelse((hf$count >= 30 & hf$frz == "freeze"), TRUE, NA)

hf.count<- select(hf, Year, fs)
hf.count<-na.omit(hf.count)
hf.count<-as.data.frame(table(hf.count$year))

# Augsburg, Germany: 48.4264N 10.9431E
lat1<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "AUGSBURG GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat1$year <- substr(lat1$date, 0, 4)
lat1<- filter(lat1, year>=1986)
lat1$month<- substr(lat1$date, 5, 6)
lat1$day<- substr(lat1$date, 7,8)
lat1<- lat1 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat1$doy<-yday(lat1$date)
lat1$year<-substr(lat1$date,0,4)
lat1$gdd <- lat1$Tmax - 10
lat1$gdd <-ifelse(lat1$gdd>0, lat1$gdd, NA)
lat1$warm<- ifelse(!is.na(lat1$gdd), 1, 0)
lat1$frz<- ifelse((lat1$Tmin<=-2.2), "freeze", "thaw")
lat1$count <- ave(
  lat1$warm, lat1$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat1<- lat1 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat1$fs<- ifelse((lat1$count >= 40 & lat1$frz == "freeze"), TRUE, NA)
write.csv(lat1, file="~/Documents/git/springfreeze/output/augs.csv", row.names =FALSE)

aug.count<- select(lat1, year, fs)
aug.count<-na.omit(aug.count)
aug.count<-as.data.frame(table(aug.count$year))

# Bamberg,Germany: 49.8753N 10.9217E
lat2<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BAMBERG GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat2$year <- substr(lat2$date, 0, 4)
lat2<- filter(lat2, year>=1986)
lat2$month<- substr(lat2$date, 5, 6)
lat2$day<- substr(lat2$date, 7,8)
lat2<- lat2 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat2$doy<-yday(lat2$date)
lat2$year<-substr(lat2$date,0,4)
lat2$gdd <- lat2$Tmax - 10
lat2$gdd <-ifelse(lat2$gdd>0, lat2$gdd, NA)
lat2$warm<- ifelse(!is.na(lat2$gdd), 1, 0)
lat2$frz<- ifelse((lat2$Tmin<=-2.2), "freeze", "thaw")
lat2$count <- ave(
  lat2$warm, lat2$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat2<- lat2 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat2$fs<- ifelse((lat2$count >= 40 & lat2$frz == "freeze"), TRUE, NA)

bam.count<- select(lat2, year, fs)
bam.count<-na.omit(bam.count)
bam.count<-as.data.frame(table(bam.count$year))

# Bremen, Germany: 53.0464N 8.7992E
lat3<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BREMEN GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat3$year <- substr(lat3$date, 0, 4)
lat3<- filter(lat3, year>=1986)
lat3$month<- substr(lat3$date, 5, 6)
lat3$day<- substr(lat3$date, 7,8)
lat3<- lat3 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat3$doy<-yday(lat3$date)
lat3$year<-substr(lat3$date,0,4)
lat3$gdd <- lat3$Tmax - 10
lat3$gdd <-ifelse(lat3$gdd>0, lat3$gdd, NA)
lat3$warm<- ifelse(!is.na(lat3$gdd), 1, 0)
lat3$frz<- ifelse((lat3$Tmin<=-2.2), "freeze", "thaw")
lat3$count <- ave(
  lat3$warm, lat3$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat3<- lat3 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat3$fs<- ifelse((lat3$count >= 40 & lat3$frz == "freeze"), TRUE, NA)

bre.count<- select(lat3, year, fs)
bre.count<-na.omit(bre.count)
bre.count<-as.data.frame(table(bre.count$year))

# Hamburg, Germany: 53.635N 9.99E
lat4<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HAMBURG FUHLSBUETTEL GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat4$year <- substr(lat4$date, 0, 4)
lat4<- filter(lat4, year>=1986)
lat4$month<- substr(lat4$date, 5, 6)
lat4$day<- substr(lat4$date, 7,8)
lat4<- lat4 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat4$doy<-yday(lat4$date)
lat4$year<-substr(lat4$date,0,4)
lat4$gdd <- lat4$Tmax - 10
lat4$gdd <-ifelse(lat4$gdd>0, lat4$gdd, NA)
lat4$warm<- ifelse(!is.na(lat4$gdd), 1, 0)
lat4$frz<- ifelse((lat4$Tmin<=-2.2), "freeze", "thaw")
lat4$count <- ave(
  lat4$warm, lat4$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat4<- lat4 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat4$fs<- ifelse((lat4$count >= 40 & lat4$frz == "freeze"), TRUE, NA)

ham.count<- select(lat4, year, fs)
ham.count<-na.omit(ham.count)
ham.count<-as.data.frame(table(ham.count$year))

# Erfurt, Germany: 50.9844N 10.9631E
lat5<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ERFURT BINDERSLEBEN GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat5$year <- substr(lat5$date, 0, 4)
lat5<- filter(lat5, year>=1986)
lat5$month<- substr(lat5$date, 5, 6)
lat5$day<- substr(lat5$date, 7,8)
lat5<- lat5 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat5$doy<-yday(lat5$date)
lat5$year<-substr(lat5$date,0,4)
lat5$gdd <- lat5$Tmax - 10
lat5$gdd <-ifelse(lat5$gdd>0, lat5$gdd, NA)
lat5$warm<- ifelse(!is.na(lat5$gdd), 1, 0)
lat5$frz<- ifelse((lat5$Tmin<=-2.2), "freeze", "thaw")
lat5$count <- ave(
  lat5$warm, lat5$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat5<- lat5 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat5$fs<- ifelse((lat5$count >= 40 & lat5$frz == "freeze"), TRUE, NA)

erf.count<- select(lat5, year, fs)
erf.count<-na.omit(erf.count)
erf.count<-as.data.frame(table(erf.count$year))

# Oslo Blindern, Norway: 59.9428N 10.7206E
lat6<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "OSLO BLINDERN NO") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat6$year <- substr(lat6$date, 0, 4)
lat6<- filter(lat6, year>=1986)
lat6$month<- substr(lat6$date, 5, 6)
lat6$day<- substr(lat6$date, 7,8)
lat6<- lat6 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat6$doy<-yday(lat6$date)
lat6$year<-substr(lat6$date,0,4)
lat6$gdd <- lat6$Tmax - 10
lat6$gdd <-ifelse(lat6$gdd>0, lat6$gdd, NA)
lat6$warm<- ifelse(!is.na(lat6$gdd), 1, 0)
lat6$frz<- ifelse((lat6$Tmin<=-2.2), "freeze", "thaw")
lat6$count <- ave(
  lat6$warm, lat6$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat6<- lat6 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat6$fs<- ifelse((lat6$count >= 40 & lat6$frz == "freeze"), TRUE, NA)

os.count<- select(lat6, year, fs)
os.count<-na.omit(os.count)
os.count<-as.data.frame(table(os.count$year))

# Schleswig, Germany: 54.5289N 9.5492E
lat7<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "SCHLESWIG GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat7$year <- substr(lat7$date, 0, 4)
lat7<- filter(lat7, year>=1986)
lat7$month<- substr(lat7$date, 5, 6)
lat7$day<- substr(lat7$date, 7,8)
lat7<- lat7 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat7$doy<-yday(lat7$date)
lat7$year<-substr(lat7$date,0,4)
lat7$gdd <- lat7$Tmax - 10
lat7$gdd <-ifelse(lat7$gdd>0, lat7$gdd, NA)
lat7$warm<- ifelse(!is.na(lat7$gdd), 1, 0)
lat7$frz<- ifelse((lat7$Tmin<=-2.2), "freeze", "thaw")
lat7$count <- ave(
  lat7$warm, lat7$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat7<- lat7 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat7$fs<- ifelse((lat7$count >= 40 & lat7$frz == "freeze"), TRUE, NA)

sch.count<- select(lat7, year, fs)
sch.count<-na.omit(sch.count)
sch.count<-as.data.frame(table(sch.count$year))

# Kempten, Germany: 47.7242N 10.3364E
lat8<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "KEMPTEN GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat8$year <- substr(lat8$date, 0, 4)
lat8<- filter(lat8, year>=1986)
lat8$month<- substr(lat8$date, 5, 6)
lat8$day<- substr(lat8$date, 7,8)
lat8<- lat8 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat8$doy<-yday(lat8$date)
lat8$year<-substr(lat8$date,0,4)
lat8$gdd <- lat8$Tmax - 10
lat8$gdd <-ifelse(lat8$gdd>0, lat8$gdd, NA)
lat8$warm<- ifelse(!is.na(lat8$gdd), 1, 0)
lat8$frz<- ifelse((lat8$Tmin<=-2.2), "freeze", "thaw")
lat8$count <- ave(
  lat8$warm, lat8$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat8<- lat8 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat8$fs<- ifelse((lat8$count >= 40 & lat8$frz == "freeze"), TRUE, NA)

kem.count<- select(lat8, year, fs)
kem.count<-na.omit(kem.count)
kem.count<-as.data.frame(table(kem.count$year))

# Hannover, Germany: 52.47N 9.68E
lat9<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HANNOVER GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat9$year <- substr(lat9$date, 0, 4)
lat9<- filter(lat9, year>=1986)
lat9$month<- substr(lat9$date, 5, 6)
lat9$day<- substr(lat9$date, 7,8)
lat9<- lat9 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat9$doy<-yday(lat9$date)
lat9$year<-substr(lat9$date,0,4)
lat9$gdd <- lat9$Tmax - 10
lat9$gdd <-ifelse(lat9$gdd>0, lat9$gdd, NA)
lat9$warm<- ifelse(!is.na(lat9$gdd), 1, 0)
lat9$frz<- ifelse((lat9$Tmin<=-2.2), "freeze", "thaw")
lat9$count <- ave(
  lat9$warm, lat9$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat9<- lat9 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat9$fs<- ifelse((lat9$count >= 40 & lat9$frz == "freeze"), TRUE, NA)

han.count<- select(lat9, year, fs)
han.count<-na.omit(han.count)
han.count<-as.data.frame(table(han.count$year))

# Jena, Germany: 50.9267N 11.5842E
lat10<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "JENA STERNWARTE GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat10$year <- substr(lat10$date, 0, 4)
lat10<- filter(lat10, year>=1986)
lat10$month<- substr(lat10$date, 5, 6)
lat10$day<- substr(lat10$date, 7,8)
lat10<- lat10 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat10$doy<-yday(lat10$date)
lat10$year<-substr(lat10$date,0,4)
lat10$gdd <- lat10$Tmax - 10
lat10$gdd <-ifelse(lat10$gdd>0, lat10$gdd, NA)
lat10$warm<- ifelse(!is.na(lat10$gdd), 1, 0)
lat10$frz<- ifelse((lat10$Tmin<=-2.2), "freeze", "thaw")
lat10$count <- ave(
  lat10$warm, lat10$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat10<- lat10 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat10$fs<- ifelse((lat10$count >= 40 & lat10$frz == "freeze"), TRUE, NA)

jen.count<- select(lat10, year, fs)
jen.count<-na.omit(jen.count)
jen.count<-as.data.frame(table(jen.count$year))

# Flyvestation, Denmark: 57.093N 9.849E
lat11<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "FLYVESTATION AALBORG DA") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat11$year <- substr(lat11$date, 0, 4)
lat11<- filter(lat11, year>=1986)
lat11$month<- substr(lat11$date, 5, 6)
lat11$day<- substr(lat11$date, 7,8)
lat11<- lat11 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat11$doy<-yday(lat11$date)
lat11$year<-substr(lat11$date,0,4)
lat11$gdd <- lat11$Tmean - 10
lat11$gdd <-ifelse(lat11$gdd>0, lat11$gdd, NA)
lat11$warm<- ifelse(!is.na(lat11$gdd), 1, 0)
lat11$frz<- ifelse((lat11$Tmean<=0), "freeze", "thaw")
lat11$count <- ave(
  lat11$warm, lat11$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat11<- lat11 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat11$fs<- ifelse((lat11$count >= 40 & lat11$frz == "freeze"), TRUE, NA)

fly.count<- select(lat11, year, fs)
fly.count<-na.omit(fly.count)
fly.count<-as.data.frame(table(fly.count$year))

# Anthony, KS, USA: 37.15611N -98.01667
am1<-america %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ANTHONY KS US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am1$year <- substr(am1$date, 0, 4)
am1<- filter(am1, year>=1986)
am1$month<- substr(am1$date, 5, 6)
am1$day<- substr(am1$date, 7,8)
am1<- am1 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
am1$doy<-yday(am1$date)
am1$year<-substr(am1$date,0,4)
am1$gdd <- am1$Tmax - 10
am1$gdd <-ifelse(am1$gdd>0, am1$gdd, NA)
am1$warm<- ifelse(!is.na(am1$gdd), 1, 0)
am1$frz<- ifelse((am1$Tmin<=-2.2), "freeze", "thaw")
am1$count <- ave(
  am1$warm, am1$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am1<- am1 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am1$fs<- ifelse((am1$count >= 50 & am1$frz == "freeze"), TRUE, NA)

anth.count<- select(am1, year, fs)
anth.count<-na.omit(anth.count)
anth.count<-as.data.frame(table(anth.count$year))

# West Point, NE, USA: 41.85N -96.71667E
am3<-america %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "WEST POINT NE US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am3$year <- substr(am3$date, 0, 4)
am3<- filter(am3, year>=1986)
am3$month<- substr(am3$date, 5, 6)
am3$day<- substr(am3$date, 7,8)
am3<- am3 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
am3$doy<-yday(am3$date)
am3$year<-substr(am3$date,0,4)
am3$gdd <- am3$Tmax - 10
am3$gdd <-ifelse(am3$gdd>0, am3$gdd, NA)
am3$warm<- ifelse(!is.na(am3$gdd), 1, 0)
am3$frz<- ifelse((am3$Tmin<=-2.2), "freeze", "thaw")
am3$count <- ave(
  am3$warm, am3$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am3<- am3 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am3$fs<- ifelse((am3$count >= 50 & am3$frz == "freeze"), TRUE, NA)

west.count<- select(am3, year, fs)
west.count<-na.omit(west.count)
west.count<-as.data.frame(table(west.count$year))

# Brookings NE, USA: 44.31667N -96.76667E
am4<-america %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BROOKINGS 2 NE SD US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am4$year <- substr(am4$date, 0, 4)
am4<- filter(am4, year>=1986)
am4$month<- substr(am4$date, 5, 6)
am4$day<- substr(am4$date, 7,8)
am4<- am4 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
am4$doy<-yday(am4$date)
am4$year<-substr(am4$date,0,4)
am4$gdd <- am4$Tmax - 10
am4$gdd <-ifelse(am4$gdd>0, am4$gdd, NA)
am4$warm<- ifelse(!is.na(am4$gdd), 1, 0)
am4$frz<- ifelse((am4$Tmin<=-2.2), "freeze", "thaw")
am4$count <- ave(
  am4$warm, am4$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am4<- am4 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am4$fs<- ifelse((am4$count >= 50 & am4$frz == "freeze"), TRUE, NA)

bro.count<- select(am4, year, fs)
bro.count<-na.omit(bro.count)
bro.count<-as.data.frame(table(bro.count$year))

# Aberdeen, SD, USA: 45.45N -98.4333E
am5<-america %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ABERDEEN REGIONAL AIRPORT SD US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am5$year <- substr(am5$date, 0, 4)
am5<- filter(am5, year>=1986)
am5$month<- substr(am5$date, 5, 6)
am5$day<- substr(am5$date, 7,8)
am5<- am5 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
am5$doy<-yday(am5$date)
am5$year<-substr(am5$date,0,4)
am5$gdd <- am5$Tmax - 10
am5$gdd <-ifelse(am5$gdd>0, am5$gdd, NA)
am5$warm<- ifelse(!is.na(am5$gdd), 1, 0)
am5$frz<- ifelse((am5$Tmin<=-2.2), "freeze", "thaw")
am5$count <- ave(
  am5$warm, am5$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am5<- am5 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am5$fs<- ifelse((am5$count >= 50 & am5$frz == "freeze"), TRUE, NA)

abe.count<- select(am5, year, fs)
abe.count<-na.omit(abe.count)
abe.count<-as.data.frame(table(abe.count$year))

# Pembina, ND, USA: 48.96667N -97.2333E
am7<-america %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "PEMBINA ND US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am7$year <- substr(am7$date, 0, 4)
am7<- filter(am7, year>=1986)
am7$month<- substr(am7$date, 5, 6)
am7$day<- substr(am7$date, 7,8)
am7<- am7 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
am7$doy<-yday(am7$date)
am7$year<-substr(am7$date,0,4)
am7$gdd <- am7$Tmax - 10
am7$gdd <-ifelse(am7$gdd>0, am7$gdd, NA)
am7$warm<- ifelse(!is.na(am7$gdd), 1, 0)
am7$frz<- ifelse((am7$Tmin<=-2.2), "freeze", "thaw")
am7$count <- ave(
  am7$warm, am7$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am7<- am7 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am7$fs<- ifelse((am7$count >= 50 & am7$frz == "freeze"), TRUE, NA)

pem.count<- select(am7, year, fs)
pem.count<-na.omit(pem.count)
pem.count<-as.data.frame(table(pem.count$year))

# Hastings, NE, USA: 40.58333N -98.35
am8<-italy %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HASTINGS 4 N NE US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am8$year <- substr(am8$date, 0, 4)
am8<- filter(am8, year>=1986)
am8$month<- substr(am8$date, 5, 6)
am8$day<- substr(am8$date, 7,8)
am8<- am8 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
am8$doy<-yday(am8$date)
am8$year<-substr(am8$date,0,4)
am8$gdd <- am8$Tmax - 10
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, NA)
am8$warm<- ifelse(!is.na(am8$gdd), 1, 0)
am8$frz<- ifelse((am8$Tmin<=-2.2), "freeze", "thaw")
am8$count <- ave(
  am8$warm, am8$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am8<- am8 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am8$fs<- ifelse((am8$count >= 50 & am8$frz == "freeze"), TRUE, NA)

has.count<- select(am8, year, fs)
has.count<-na.omit(has.count)
has.count<-as.data.frame(table(has.count$year))

# Yankton, SD, USA: 42.88333N -97.35
am9<-italy %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "YANKTON SD US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am9$year <- substr(am9$date, 0, 4)
am9<- filter(am9, year>=1986)
am9$month<- substr(am9$date, 5, 6)
am9$day<- substr(am9$date, 7,8)
am9<- am9 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
am9$doy<-yday(am9$date)
am9$year<-substr(am9$date,0,4)
am9$gdd <- am9$Tmax - 10
am9$gdd <-ifelse(am9$gdd>0, am9$gdd, NA)
am9$warm<- ifelse(!is.na(am9$gdd), 1, 0)
am9$frz<- ifelse((am9$Tmin<=-2.2), "freeze", "thaw")
am9$count <- ave(
  am9$warm, am9$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am9<- am9 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am9$fs<- ifelse((am9$count >= 50 & am9$frz == "freeze"), TRUE, NA)

yan.count<- select(am9, year, fs)
yan.count<-na.omit(yan.count)
yan.count<-as.data.frame(table(yan.count$year))

# Grand Forks, ND, USA: 47.9333N -97.08333E
am10<-kansas %>%
  select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "GRAND FORKS UNIVERSITY NWS ND US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am10$year <- substr(am10$date, 0, 4)
am10<- filter(am10, year>=1986)
am10$month<- substr(am10$date, 5, 6)
am10$day<- substr(am10$date, 7,8)
am10<- am10 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmin, Tmax)
am10$doy<-yday(am10$date)
am10$year<-substr(am10$date,0,4)
am10$gdd <- am10$Tmax - 10
am10$gdd <-ifelse(am10$gdd>0, am10$gdd, NA)
am10$warm<- ifelse(!is.na(am10$gdd), 1, 0)
am10$frz<- ifelse((am10$Tmin<=-2.2), "freeze", "thaw")
am10$count <- ave(
  am10$warm, am10$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am10<- am10 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
am10$fs<- ifelse((am10$count >= 50 & am10$frz == "freeze"), TRUE, NA)

gra.count<- select(am10, year, fs)
gra.count<-na.omit(gra.count)
gra.count<-as.data.frame(table(gra.count$year))

# 14 Dec 2016 - Cat
## Investigating the stats with the latitudinal weather data

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
europe<-read.csv("input/europe.lat.csv", header=TRUE)
america<-read.csv("input/america.lat.csv", header=TRUE)

# Linear Models looking at latitude and false springs
eur.lm<-lm(europe$Latitude~europe$False.Springs)
display(eur.lm)
am.lm<-lm(america$Latitude~america$False.Springs)
display(am.lm)
