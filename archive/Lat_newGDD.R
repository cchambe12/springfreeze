## 1 March 2017
# Weather Data Script (based off of Weather_Latitude.R) - Cat
# Aim: To create a table that indicates prevalence of FSI events by latitude
# by simply analyzing weather data
# Starting with our sites, hoping to apply to site further north and south
# Looking to change my calculation for GDD from original script!

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

# Augsburg, Germany: 48.4264N 10.9431E
lat1<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "AUGSBURG GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat1$year <- substr(lat1$date, 0, 4)
lat1<- lat1%>%
  filter(year>=1986) %>%
  filter(year<2016)
lat1$month<- substr(lat1$date, 5, 6)
lat1$day<- substr(lat1$date, 7,8)
lat1<- lat1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat1$doy<-yday(lat1$date)
lat1$year<-substr(lat1$date,0,4)
lat1<- lat1 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat1$gdd <- lat1$Tmax - 5
lat1$gdd <-ifelse(lat1$gdd>0, lat1$gdd, 0)
lat1$frz<- ifelse((lat1$Tmin<=-3), "freeze", "thaw")
lat1$count <- ave(
  lat1$gdd, lat1$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat1$fs<- ifelse((lat1$count >= 250 & lat1$frz == "freeze"), TRUE, NA)
#write.csv(lat1, file="~/Documents/git/springfreeze/output/augs.csv", row.names =FALSE)

aug.count<- dplyr::select(lat1, year, fs)
aug.count<-na.omit(aug.count)
aug.count<-as.data.frame(table(aug.count$year))

# Bamberg,Germany: 49.8603N 10.9217E
lat2<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BAMBERG GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat2$year <- substr(lat2$date, 0, 4)
lat2<- lat2 %>%
  filter(year>=1986) %>%
  filter(year<2016)
lat2$month<- substr(lat2$date, 5, 6)
lat2$day<- substr(lat2$date, 7,8)
lat2<- lat2 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat2$doy<-yday(lat2$date)
lat2$year<-substr(lat2$date,0,4)
lat2<- lat2 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat2$gdd <- lat2$Tmax - 5
lat2$gdd <-ifelse(lat2$gdd>0, lat2$gdd, 0)
lat2$frz<- ifelse((lat2$Tmin<=-3), "freeze", "thaw")
lat2$count <- ave(
  lat2$gdd, lat2$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat2$fs<- ifelse((lat2$count >= 250 & lat2$frz == "freeze"), TRUE, NA)

bam.count<- dplyr::select(lat2, year, fs)
bam.count<-na.omit(bam.count)
bam.count<-as.data.frame(table(bam.count$year))

# Bremen, Germany: 53.0464N 8.7992E
lat3<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BREMEN GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat3$year <- substr(lat3$date, 0, 4)
lat3<- filter(lat3, year>=1986)%>%
  filter(year<2016)
lat3$month<- substr(lat3$date, 5, 6)
lat3$day<- substr(lat3$date, 7,8)
lat3<- lat3 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat3$doy<-yday(lat3$date)
lat3$year<-substr(lat3$date,0,4)
lat3<- lat3 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat3$gdd <- lat3$Tmax - 5
lat3$gdd <-ifelse(lat3$gdd>0, lat3$gdd, 0)
lat3$frz<- ifelse((lat3$Tmin<=-3), "freeze", "thaw")
lat3$count <- ave(
  lat3$gdd, lat3$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat3$fs<- ifelse((lat3$count >= 250 & lat3$frz == "freeze"), TRUE, NA)

bre.count<- dplyr::select(lat3, year, fs)
bre.count<-na.omit(bre.count)
bre.count<-as.data.frame(table(bre.count$year))

# Hamburg, Germany: 53.635N 9.99E
lat4<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HAMBURG FUHLSBUETTEL GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat4$year <- substr(lat4$date, 0, 4)
lat4<- lat4 %>% 
  filter(year>=1986) %>%
  filter(year<2016)
lat4$month<- substr(lat4$date, 5, 6)
lat4$day<- substr(lat4$date, 7,8)
lat4<- lat4 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat4$doy<-yday(lat4$date)
lat4$year<-substr(lat4$date,0,4)
lat4<- lat4 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat4$gdd <- lat4$Tmax - 5
lat4$gdd <-ifelse(lat4$gdd>0, lat4$gdd, 0)
lat4$frz<- ifelse((lat4$Tmin<=-3), "freeze", "thaw")
lat4$count <- ave(
  lat4$gdd, lat4$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat4$fs<- ifelse((lat4$count >= 250 & lat4$frz == "freeze"), TRUE, NA)

ham.count<- dplyr::select(lat4, year, fs)
ham.count<-na.omit(ham.count)
ham.count<-as.data.frame(table(ham.count$year))

# Oslo Blindern, Norway: 59.9428N 10.7206E
lat6<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "OSLO BLINDERN NO") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat6$year <- substr(lat6$date, 0, 4)
lat6<- lat6 %>% 
  filter(year>=1986) %>%
  filter(year<2016)
lat6$month<- substr(lat6$date, 5, 6)
lat6$day<- substr(lat6$date, 7,8)
lat6<- lat6 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat6$doy<-yday(lat6$date)
lat6$year<-substr(lat6$date,0,4)
lat6<- lat6 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat6$gdd <- lat6$Tmax - 5
lat6$gdd <-ifelse(lat6$gdd>0, lat6$gdd, 0)
lat6$frz<- ifelse((lat6$Tmin<=-3), "freeze", "thaw")
lat6$count <- ave(
  lat6$gdd, lat6$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat6$fs<- ifelse((lat6$count >= 250 & lat6$frz == "freeze"), TRUE, NA)

os.count<- dplyr::select(lat6, year, fs)
os.count<-na.omit(os.count)
os.count<-as.data.frame(table(os.count$year))

# Schleswig, Germany: 54.5289N 9.5492E
lat7<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "SCHLESWIG GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat7$year <- substr(lat7$date, 0, 4)
lat7<- lat7
  filter(year>=1986) %>%
  filter(year<2016)
lat7$month<- substr(lat7$date, 5, 6)
lat7$day<- substr(lat7$date, 7,8)
lat7<- lat7 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat7$doy<-yday(lat7$date)
lat7$year<-substr(lat7$date,0,4)
lat7<- lat7 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat7$gdd <- lat7$Tmax - 5
lat7$gdd <-ifelse(lat7$gdd>0, lat7$gdd, 0)
lat7$frz<- ifelse((lat7$Tmin<=-3), "freeze", "thaw")
lat7$count <- ave(
  lat7$gdd, lat7$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat7$fs<- ifelse((lat7$count >= 250 & lat7$frz == "freeze"), TRUE, NA)

sch.count<- dplyr::select(lat7, year, fs)
sch.count<-na.omit(sch.count)
sch.count<-as.data.frame(table(sch.count$year))

# Kempten, Germany: 47.7242N 10.3364E
lat8<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "KEMPTEN GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat8$year <- substr(lat8$date, 0, 4)
lat8<- lat8 %>%
  filter(year>=1986) %>%
  filter(year<2016)
lat8$month<- substr(lat8$date, 5, 6)
lat8$day<- substr(lat8$date, 7,8)
lat8<- lat8 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat8$doy<-yday(lat8$date)
lat8$year<-substr(lat8$date,0,4)
lat8<- lat8 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat8$gdd <- lat8$Tmax - 5
lat8$gdd <-ifelse(lat8$gdd>0, lat8$gdd, 0)
lat8$frz<- ifelse((lat8$Tmin<=-3), "freeze", "thaw")
lat8$count <- ave(
  lat8$gdd, lat8$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat8$fs<- ifelse((lat8$count >= 250 & lat8$frz == "freeze"), TRUE, NA)

kem.count<- dplyr::select(lat8, year, fs)
kem.count<-na.omit(kem.count)
kem.count<-as.data.frame(table(kem.count$year))

# Hannover, Germany: 52.47N 9.68E
lat9<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HANNOVER GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat9$year <- substr(lat9$date, 0, 4)
lat9<- lat9 %>%
  filter(year>=1986) %>%
  filter(year<2016)
lat9$month<- substr(lat9$date, 5, 6)
lat9$day<- substr(lat9$date, 7,8)
lat9<- lat9 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat9$doy<-yday(lat9$date)
lat9$year<-substr(lat9$date,0,4)
lat9<- lat9 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat9$gdd <- lat9$Tmax - 5
lat9$gdd <-ifelse(lat9$gdd>0, lat9$gdd, 0)
lat9$frz<- ifelse((lat9$Tmin<=-3), "freeze", "thaw")
lat9$count <- ave(
  lat9$gdd, lat9$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat9$fs<- ifelse((lat9$count >= 250 & lat9$frz == "freeze"), TRUE, NA)

han.count<- dplyr::select(lat9, year, fs)
han.count<-na.omit(han.count)
han.count<-as.data.frame(table(han.count$year))

# Jena, Germany: 50.9267N 11.5842E
lat10<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "JENA STERNWARTE GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat10$year <- substr(lat10$date, 0, 4)
lat10<- lat10 %>%
  filter(year>=1986) %>%
  filter(year<2016)
lat10$month<- substr(lat10$date, 5, 6)
lat10$day<- substr(lat10$date, 7,8)
lat10<- lat10 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat10$doy<-yday(lat10$date)
lat10$year<-substr(lat10$date,0,4)
lat10<- lat10 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat10$gdd <- lat10$Tmax - 5
lat10$gdd <-ifelse(lat10$gdd>0, lat10$gdd, 0)
lat10$frz<- ifelse((lat10$Tmin<=-3), "freeze", "thaw")
lat10$count <- ave(
  lat10$gdd, lat10$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat10$fs<- ifelse((lat10$count >= 250 & lat10$frz == "freeze"), TRUE, NA)

jen.count<- dplyr::select(lat10, year, fs)
jen.count<-na.omit(jen.count)
jen.count<-as.data.frame(table(jen.count$year))

# Flyvestation, Denmark: 57.093N 9.849E - PROBABLY DON'T WANT TO INCLUDE!!
lat11<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "FLYVESTATION AALBORG DA") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat11$year <- substr(lat11$date, 0, 4)
lat11<- lat11 %>% 
  filter(year>=1986) %>%
  filter(year<2016)
lat11$month<- substr(lat11$date, 5, 6)
lat11$day<- substr(lat11$date, 7,8)
lat11<- lat11 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmean, Tmin, Tmax)
lat11$doy<-yday(lat11$date)
lat11$year<-substr(lat11$date,0,4)
lat11<- lat11 %>%
  filter(doy >= 60) %>%
  filter(doy <= 150)
lat11$gdd <- lat11$Tmean - 10
lat11$gdd <-ifelse(lat11$gdd>0, lat11$gdd, 0)
lat11$frz<- ifelse((lat11$Tmean<=-2), "freeze", "thaw")
lat11$count <- ave(
  lat11$gdd, lat11$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

lat11$fs<- ifelse((lat11$count >= 250 & lat11$frz == "freeze"), TRUE, NA)

fly.count<- dplyr::select(lat11, year, fs)
fly.count<-na.omit(fly.count)
fly.count<-as.data.frame(table(fly.count$year))

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
  filter(year>=1986) %>%
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
  filter(year>=1986) %>%
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
  filter(year>=1986) %>%
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
  filter(year>=1986) %>%
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
  filter(year>=1986) %>%
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
am8<-italy %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HASTINGS 4 N NE US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am8$year <- substr(am8$date, 0, 4)
am8<- am8 %>% 
  filter(year>=1986) %>%
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
am9<-italy %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "YANKTON SD US") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am9$year <- substr(am9$date, 0, 4)
am9<- am9 %>% 
  filter(year>=1986) %>%
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
am10<-kansas %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "GRAND FORKS UNIVERSITY NWS ND US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am10$year <- substr(am10$date, 0, 4)
am10<- am10 %>% 
  filter(year>=1986)%>%
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