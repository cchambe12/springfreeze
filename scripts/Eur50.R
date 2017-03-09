## 2 March 2017 : European Transect (Germany to Norway)
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
setwd("~/Documents/git/springfreeze")
lat<-read.csv("input/NOAA_Eur50.csv", header=TRUE)

# Augsburg, Germany: 48.4264N 10.9431E
lat1<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "AUGSBURG GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat1$year <- substr(lat1$date, 0, 4)
lat1<- lat1%>%
  filter(year>=1965) %>%
  filter(year<2016)
lat1$month<- substr(lat1$date, 5, 6)
lat1$day<- substr(lat1$date, 7,8)
lat1<- lat1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat1$doy<-yday(lat1$date)
lat1$year<-substr(lat1$date,0,4)
lat1$Tmean <- (lat1$Tmax + lat1$Tmin)/2
lat1$gdd <- lat1$Tmean - 5
lat1$gdd <-ifelse(lat1$gdd>0, lat1$gdd, 0)
lat1$frz<- ifelse((lat1$Tmin<=-5), "freeze", "thaw")
lat1$count <- ave(
  lat1$gdd, lat1$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat1<- lat1 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat1$fs<- ifelse((lat1$count >= 100 & lat1$frz == "freeze" & lat1$count<=400), TRUE, NA)
#write.csv(lat1, file="~/Documents/git/springfreeze/output/augs.csv", row.names =FALSE)

aug.count<- dplyr::select(lat1, year, fs)
aug.count<-na.omit(aug.count)
aug.count<-as.data.frame(table(aug.count$year))

# Bamberg,Germany: 49.8903N 10.9217E
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
lat2$Tmean <- (lat2$Tmax + lat2$Tmin)/2
lat2$gdd <- lat2$Tmean - 5
lat2$gdd <-ifelse(lat2$gdd>0, lat2$gdd, 0)
lat2$frz<- ifelse((lat2$Tmin<=-5), "freeze", "thaw")
lat2$count <- ave(
  lat2$gdd, lat2$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat2<- lat2 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat2$fs<- ifelse((lat2$count >= 100 & lat2$frz == "freeze" & lat2$count<=400), TRUE, NA)

bam.count<- dplyr::select(lat2, year, fs)
bam.count<-na.omit(bam.count)
bam.count<-as.data.frame(table(bam.count$year))

# Bremen, Germany: 53.0464N 8.7992E
lat3<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BREMEN GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat3$year <- substr(lat3$date, 0, 4)
lat3<- filter(lat3, year>=1965)%>%
  filter(year<2016)
lat3$month<- substr(lat3$date, 5, 6)
lat3$day<- substr(lat3$date, 7,8)
lat3<- lat3 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat3$doy<-yday(lat3$date)
lat3$year<-substr(lat3$date,0,4)
lat3$Tmean <- (lat3$Tmax + lat3$Tmin)/2
lat3$gdd <- lat3$Tmean - 5
lat3$gdd <-ifelse(lat3$gdd>0, lat3$gdd, 0)
lat3$frz<- ifelse((lat3$Tmin<=-5), "freeze", "thaw")
lat3$count <- ave(
  lat3$gdd, lat3$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat3<- lat3 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat3$fs<- ifelse((lat3$count >= 100 & lat3$frz == "freeze" & lat3$count<=400), TRUE, NA)

bre.count<- dplyr::select(lat3, year, fs)
bre.count<-na.omit(bre.count)
bre.count<-as.data.frame(table(bre.count$year))

# Hamburg, Germany: 53.635N 9.99E
lat4<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HAMBURG FUHLSBUETTEL GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat4$year <- substr(lat4$date, 0, 4)
lat4<- lat4 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
lat4$month<- substr(lat4$date, 5, 6)
lat4$day<- substr(lat4$date, 7,8)
lat4<- lat4 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat4$doy<-yday(lat4$date)
lat4$year<-substr(lat4$date,0,4)
lat4$Tmean <- (lat4$Tmax + lat4$Tmin)/2
lat4$gdd <- lat4$Tmean - 5
lat4$gdd <-ifelse(lat4$gdd>0, lat4$gdd, 0)
lat4$frz<- ifelse((lat4$Tmin<=-5), "freeze", "thaw")
lat4$count <- ave(
  lat4$gdd, lat4$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat4<- lat4 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat4$fs<- ifelse((lat4$count >= 100 & lat4$frz == "freeze" & lat4$count<=400), TRUE, NA)

ham.count<- dplyr::select(lat4, year, fs)
ham.count<-na.omit(ham.count)
ham.count<-as.data.frame(table(ham.count$year))

# Oslo Blindern, Norway: 59.9428N 10.7206E
lat6<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "OSLO BLINDERN NO") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat6$year <- substr(lat6$date, 0, 4)
lat6<- lat6 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
lat6$month<- substr(lat6$date, 5, 6)
lat6$day<- substr(lat6$date, 7,8)
lat6<- lat6 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat6$doy<-yday(lat6$date)
lat6$year<-substr(lat6$date,0,4)
lat6$Tmean <- (lat6$Tmax + lat6$Tmin)/2
lat6$gdd <- lat6$Tmean - 5
lat6$gdd <-ifelse(lat6$gdd>0, lat6$gdd, 0)
lat6$frz<- ifelse((lat6$Tmin<=-5), "freeze", "thaw")
lat6$count <- ave(
  lat6$gdd, lat6$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat6<- lat6 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat6$fs<- ifelse((lat6$count >= 100 & lat6$frz == "freeze" & lat6$count<=400), TRUE, NA)

os.count<- dplyr::select(lat6, year, fs)
os.count<-na.omit(os.count)
os.count<-as.data.frame(table(os.count$year))

# Schleswig, Germany: 54.5289N 9.5492E
lat7<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "SCHLESWIG GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat7$year <- substr(lat7$date, 0, 4)
lat7<- lat7%>%
  filter(year>=1965) %>%
  filter(year<2016)
lat7$month<- substr(lat7$date, 5, 6)
lat7$day<- substr(lat7$date, 7,8)
lat7<- lat7 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat7$doy<-yday(lat7$date)
lat7$year<-substr(lat7$date,0,4)
lat7$Tmean <- (lat7$Tmax + lat7$Tmin)/2
lat7$gdd <- lat7$Tmean - 5
lat7$gdd <-ifelse(lat7$gdd>0, lat7$gdd, 0)
lat7$frz<- ifelse((lat7$Tmin<=-5), "freeze", "thaw")
lat7$count <- ave(
  lat7$gdd, lat7$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat7<- lat7 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat7$fs<- ifelse((lat7$count >= 100 & lat7$frz == "freeze" & lat7$count<=400), TRUE, NA)

sch.count<- dplyr::select(lat7, year, fs)
sch.count<-na.omit(sch.count)
sch.count<-as.data.frame(table(sch.count$year))

# Kempten, Germany: 47.7242N 10.3364E
lat8<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "KEMPTEN GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat8$year <- substr(lat8$date, 0, 4)
lat8<- lat8 %>%
  filter(year>=1965) %>%
  filter(year<2016)
lat8$month<- substr(lat8$date, 5, 6)
lat8$day<- substr(lat8$date, 7,8)
lat8<- lat8 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat8$doy<-yday(lat8$date)
lat8$year<-substr(lat8$date,0,4)
lat8$Tmean <- (lat8$Tmax + lat8$Tmin)/2
lat8$gdd <- lat8$Tmean - 5
lat8$gdd <-ifelse(lat8$gdd>0, lat8$gdd, 0)
lat8$frz<- ifelse((lat8$Tmin<=-5), "freeze", "thaw")
lat8$count <- ave(
  lat8$gdd, lat8$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat8<- lat8 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat8$fs<- ifelse((lat8$count >= 100 & lat8$frz == "freeze" & lat8$count<=400), TRUE, NA)

kem.count<- dplyr::select(lat8, year, fs)
kem.count<-na.omit(kem.count)
kem.count<-as.data.frame(table(kem.count$year))

# Hannover, Germany: 52.47N 9.68E
lat9<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HANNOVER GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat9$year <- substr(lat9$date, 0, 4)
lat9<- lat9 %>%
  filter(year>=1965) %>%
  filter(year<2016)
lat9$month<- substr(lat9$date, 5, 6)
lat9$day<- substr(lat9$date, 7,8)
lat9<- lat9 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat9$doy<-yday(lat9$date)
lat9$year<-substr(lat9$date,0,4)
lat9$Tmean <- (lat9$Tmax + lat9$Tmin)/2
lat9$gdd <- lat9$Tmean - 5
lat9$gdd <-ifelse(lat9$gdd>0, lat9$gdd, 0)
lat9$frz<- ifelse((lat9$Tmin<=-5), "freeze", "thaw")
lat9$count <- ave(
  lat9$gdd, lat9$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat9<- lat9 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat9$fs<- ifelse((lat9$count >= 100 & lat9$frz == "freeze" & lat9$count<=400), TRUE, NA)

han.count<- dplyr::select(lat9, year, fs)
han.count<-na.omit(han.count)
han.count<-as.data.frame(table(han.count$year))

# Jena, Germany: 50.9267N 11.5842E
lat10<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "JENA STERNWARTE GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat10$year <- substr(lat10$date, 0, 4)
lat10<- lat10 %>%
  filter(year>=1965) %>%
  filter(year<2016)
lat10$month<- substr(lat10$date, 5, 6)
lat10$day<- substr(lat10$date, 7,8)
lat10<- lat10 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
lat10$doy<-yday(lat10$date)
lat10$year<-substr(lat10$date,0,4)
lat10$Tmean <- (lat10$Tmax + lat10$Tmin)/2
lat10$gdd <- lat10$Tmean - 5
lat10$gdd <-ifelse(lat10$gdd>0, lat10$gdd, 0)
lat10$frz<- ifelse((lat10$Tmin<=-5), "freeze", "thaw")
lat10$count <- ave(
  lat10$gdd, lat10$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat10<- lat10 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat10$fs<- ifelse((lat10$count >= 100 & lat10$frz == "freeze" & lat10$count<=400), TRUE, NA)

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
  filter(year>=1965) %>%
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
  filter(doy <= 100)
lat11$gdd <- lat11$Tmean - 5
lat11$gdd <-ifelse(lat11$gdd>0, lat11$gdd, 0)
lat11$frz<- ifelse((lat11$Tmin<=-5), "freeze", "thaw")
lat11$count <- ave(
  lat11$gdd, lat11$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat11<- lat11 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat11$fs<- ifelse((lat11$count >= 100 & lat11$frz == "freeze"), TRUE, NA)

fly.count<- dplyr::select(lat11, year, fs)
fly.count<-na.omit(fly.count)
fly.count<-as.data.frame(table(fly.count$year))

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

eur.lm<-lm(europe$Latitude~europe$hf.gdd)
display(eur.lm)

eur<-ggplot(europe, aes(y=hf.gdd, x=Latitude)) + geom_point() + geom_smooth(method="lm")
