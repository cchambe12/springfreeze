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
ger.more<- read.csv("input/germany.more.csv", header=TRUE)
norway<- read.csv("input/norway.csv", header=TRUE)
denmark<- read.csv("input/denmark.csv", header=TRUE)
final<- read.csv("input/final.weather.csv", header=TRUE)
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
hf.count<-na.omit(dk.count)
hf.count<-as.data.frame(table(hf.count$year))

# Muenchen, Germany: 48.1642N, 11.5442E NOAA
lat1<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMAX, TMIN) %>%
  filter(STATION_NAME == "MUENCHEN GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmax = TMAX) %>%
  rename(Tmin = TMIN) %>%
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

mun.count<- select(lat1, year, fs)
mun.count<-na.omit(mun.count)
mun.count<-as.data.frame(table(mun.count$year))

# Augsburg, Germany: 48.4264N 10.9431E
lat2<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "AUGSBURG GM") %>%
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
write.csv(lat2, file="~/Documents/git/springfreeze/output/augs.csv", row.names =FALSE)

aug.count<- select(lat2, year, fs)
aug.count<-na.omit(aug.count)
aug.count<-as.data.frame(table(aug.count$year))

# Bamberg,Germany: 49.8753N 10.9217E
lat3<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BAMBERG GM") %>%
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

bam.count<- select(lat3, year, fs)
bam.count<-na.omit(bam.count)
bam.count<-as.data.frame(table(bam.count$year))

# Aachen, Germany: 50.7992N 6.025E - ONLY 6 YEARS
lat4<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "AACHEN ORSBACH GM") %>%
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

aa.count<- select(lat4, year, fs)
aa.count<-na.omit(aa.count)
aa.count<-as.data.frame(table(aa.count$year))

# Angermunde, Germany: 53.0331N 13.9931E
lat5<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ANGERMUNDE GM") %>%
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

ang.count<- select(lat5, year, fs)
ang.count<-na.omit(ang.count)
ang.count<-as.data.frame(table(ang.count$year))

# Bremen, Germany: 53.0464N 8.7992E
lat6<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BREMEN GM") %>%
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

bre.count<- select(lat6, year, fs)
bre.count<-na.omit(bre.count)
bre.count<-as.data.frame(table(bre.count$year))

# Hamburg, Germany: 53.635N 9.99E
lat7<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HAMBURG FUHLSBUETTEL GM") %>%
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

ham.count<- select(lat7, year, fs)
ham.count<-na.omit(ham.count)
ham.count<-as.data.frame(table(ham.count$year))

# Erfurt, Germany: 50.9844N 10.9631E
lat8<-ger.more %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "ERFURT BINDERSLEBEN GM") %>%
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

erf.count<- select(lat8, year, fs)
erf.count<-na.omit(erf.count)
erf.count<-as.data.frame(table(erf.count$year))

# Gvarv Nes, Norway: 59.3831N 9.2014E - 15 YEARS!
lat9<-norway %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "GVARV NES NO") %>%
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

gv.count<- select(lat9, year, fs)
gv.count<-na.omit(gv.count)
gv.count<-as.data.frame(table(gv.count$year))

# Oslo Blindern, Norway: 59.9428N 10.7206E
lat10<-norway %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "OSLO BLINDERN NO") %>%
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

os.count<- select(lat10, year, fs)
os.count<-na.omit(os.count)
os.count<-as.data.frame(table(os.count$year))

# Schleswig, Germany: 54.5289N 9.5492E
lat11<-lat %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "SCHLESWIG GM") %>%
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
lat11$gdd <- lat11$Tmax - 10
lat11$gdd <-ifelse(lat11$gdd>0, lat11$gdd, NA)
lat11$warm<- ifelse(!is.na(lat11$gdd), 1, 0)
lat11$frz<- ifelse((lat11$Tmin<=-2.2), "freeze", "thaw")
lat11$count <- ave(
  lat11$warm, lat11$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat11<- lat11 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat11$fs<- ifelse((lat11$count >= 40 & lat11$frz == "freeze"), TRUE, NA)

sch.count<- select(lat11, year, fs)
sch.count<-na.omit(sch.count)
sch.count<-as.data.frame(table(sch.count$year))

# Konstanz, Germany: 47.6781N 9.1908E
lat12<-denmark %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "KONSTANZ GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat12$year <- substr(lat12$date, 0, 4)
lat12<- filter(lat12, year>=1986)
lat12$month<- substr(lat12$date, 5, 6)
lat12$day<- substr(lat12$date, 7,8)
lat12<- lat12 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat12$doy<-yday(lat12$date)
lat12$year<-substr(lat12$date,0,4)
lat12$gdd <- lat12$Tmax - 10
lat12$gdd <-ifelse(lat12$gdd>0, lat12$gdd, NA)
lat12$warm<- ifelse(!is.na(lat12$gdd), 1, 0)
lat12$frz<- ifelse((lat12$Tmin<=-2.2), "freeze", "thaw")
lat12$count <- ave(
  lat12$warm, lat12$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat12<- lat12 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat12$fs<- ifelse((lat12$count >= 40 & lat12$frz == "freeze"), TRUE, NA)

kon.count<- select(lat12, year, fs)
kon.count<-na.omit(kon.count)
kon.count<-as.data.frame(table(kon.count$year))

# Kempten, Germany: 47.7242N 10.3364E
lat13<-denmark %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "KEMPTEN GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat13$year <- substr(lat13$date, 0, 4)
lat13<- filter(lat13, year>=1986)
lat13$month<- substr(lat13$date, 5, 6)
lat13$day<- substr(lat13$date, 7,8)
lat13<- lat13 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat13$doy<-yday(lat13$date)
lat13$year<-substr(lat13$date,0,4)
lat13$gdd <- lat13$Tmax - 10
lat13$gdd <-ifelse(lat13$gdd>0, lat13$gdd, NA)
lat13$warm<- ifelse(!is.na(lat13$gdd), 1, 0)
lat13$frz<- ifelse((lat13$Tmin<=-2.2), "freeze", "thaw")
lat13$count <- ave(
  lat13$warm, lat13$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat13<- lat13 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat13$fs<- ifelse((lat13$count >= 40 & lat13$frz == "freeze"), TRUE, NA)

kem.count<- select(lat13, year, fs)
kem.count<-na.omit(kem.count)
kem.count<-as.data.frame(table(kem.count$year))

# Hannover, Germany: 52.47N 9.68E
lat14<-denmark %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HANNOVER GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat14$year <- substr(lat14$date, 0, 4)
lat14<- filter(lat14, year>=1986)
lat14$month<- substr(lat14$date, 5, 6)
lat14$day<- substr(lat14$date, 7,8)
lat14<- lat14 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat14$doy<-yday(lat14$date)
lat14$year<-substr(lat14$date,0,4)
lat14$gdd <- lat14$Tmax - 10
lat14$gdd <-ifelse(lat14$gdd>0, lat14$gdd, NA)
lat14$warm<- ifelse(!is.na(lat14$gdd), 1, 0)
lat14$frz<- ifelse((lat14$Tmin<=-2.2), "freeze", "thaw")
lat14$count <- ave(
  lat14$warm, lat14$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat14<- lat14 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat14$fs<- ifelse((lat14$count >= 40 & lat14$frz == "freeze"), TRUE, NA)

han.count<- select(lat14, year, fs)
han.count<-na.omit(han.count)
han.count<-as.data.frame(table(han.count$year))

# Jena, Germany: 50.9267N 11.5842E
lat15<-final %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "JENA STERNWARTE GM") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat15$year <- substr(lat15$date, 0, 4)
lat15<- filter(lat15, year>=1986)
lat15$month<- substr(lat15$date, 5, 6)
lat15$day<- substr(lat15$date, 7,8)
lat15<- lat15 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat15$doy<-yday(lat15$date)
lat15$year<-substr(lat15$date,0,4)
lat15$gdd <- lat15$Tmax - 10
lat15$gdd <-ifelse(lat15$gdd>0, lat15$gdd, NA)
lat15$warm<- ifelse(!is.na(lat15$gdd), 1, 0)
lat15$frz<- ifelse((lat15$Tmin<=-2.2), "freeze", "thaw")
lat15$count <- ave(
  lat15$warm, lat15$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat15<- lat15 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat15$fs<- ifelse((lat15$count >= 40 & lat15$frz == "freeze"), TRUE, NA)

jen.count<- select(lat15, year, fs)
jen.count<-na.omit(jen.count)
jen.count<-as.data.frame(table(jen.count$year))

# Flyvestation, Denmark: 57.093N 9.849E
lat16<-final %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "FLYVESTATION AALBORG DA") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat16$year <- substr(lat16$date, 0, 4)
lat16<- filter(lat16, year>=1986)
lat16$month<- substr(lat16$date, 5, 6)
lat16$day<- substr(lat16$date, 7,8)
lat16<- lat16 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat16$doy<-yday(lat16$date)
lat16$year<-substr(lat16$date,0,4)
lat16$gdd <- lat16$Tmean - 10
lat16$gdd <-ifelse(lat16$gdd>0, lat16$gdd, NA)
lat16$warm<- ifelse(!is.na(lat16$gdd), 1, 0)
lat16$frz<- ifelse((lat16$Tmean<=0), "freeze", "thaw")
lat16$count <- ave(
  lat16$warm, lat16$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat16<- lat16 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat16$fs<- ifelse((lat16$count >= 40 & lat16$frz == "freeze"), TRUE, NA)

fly.count<- select(lat16, year, fs)
fly.count<-na.omit(fly.count)
fly.count<-as.data.frame(table(fly.count$year))

# Haines, Canada: 59.4503N -136.3615E
lat17<-final %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HAINES 40 NW AK CA") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat17$year <- substr(lat17$date, 0, 4)
lat17<- filter(lat17, year>=1986)
lat17$month<- substr(lat17$date, 5, 6)
lat17$day<- substr(lat17$date, 7,8)
lat17<- lat17 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat17$doy<-yday(lat17$date)
lat17$year<-substr(lat17$date,0,4)
lat17$gdd <- lat17$Tmax - 10
lat17$gdd <-ifelse(lat17$gdd>0, lat17$gdd, NA)
lat17$warm<- ifelse(!is.na(lat17$gdd), 1, 0)
lat17$frz<- ifelse((lat17$Tmin<=-2.2), "freeze", "thaw")
lat17$count <- ave(
  lat17$warm, lat17$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat17<- lat17 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat17$fs<- ifelse((lat17$count >= 40 & lat17$frz == "freeze"), TRUE, NA)

hai.count<- select(lat17, year, fs)
hai.count<-na.omit(hai.count)
hai.count<-as.data.frame(table(hai.count$year))

# Hay River, Canada: 60.8333N -115.7833E
lat18<-final %>%
  select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "HAY RIVER A CA") %>%
  rename(Tmean = TAVG) %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat18$year <- substr(lat18$date, 0, 4)
lat18<- filter(lat18, year>=1986)
lat18$month<- substr(lat18$date, 5, 6)
lat18$day<- substr(lat18$date, 7,8)
lat18<- lat18 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin, Tmax)
lat18$doy<-yday(lat18$date)
lat18$year<-substr(lat18$date,0,4)
lat18$gdd <- lat18$Tmax - 10
lat18$gdd <-ifelse(lat18$gdd>0, lat18$gdd, NA)
lat18$warm<- ifelse(!is.na(lat18$gdd), 1, 0)
lat18$frz<- ifelse((lat18$Tmin<=-2.2), "freeze", "thaw")
lat18$count <- ave(
  lat18$warm, lat18$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat18<- lat18 %>%
  filter(doy >= 60) %>%
  filter(doy <= 210)
lat18$fs<- ifelse((lat18$count >= 40 & lat18$frz == "freeze"), TRUE, NA)

hay.count<- select(lat18, year, fs)
hay.count<-na.omit(hay.count)
hay.count<-as.data.frame(table(hay.count$year))