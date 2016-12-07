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
lat1<-read.csv("input/Lat.one.csv", header=TRUE)

# Dresden-Klotz(Germany): 54.38N, 10.15E
lat1<-lat1 %>%
  select(DATE, TG, TN) %>%
  rename(Tmean = TG) %>%
  rename(Tmin = TN) %>%
  rename(date = DATE)
lat1$year <- substr(lat1$date, 0, 4)
lat1<- filter(lat1, year>=1986)
lat1$month<- substr(lat1$date, 5, 6)
lat1$day<- substr(lat1$date, 7,8)
lat1<- lat1 %>%
  select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  select(date, Tmean, Tmin)
lat1$doy<-yday(lat1$date)
lat1$year<-substr(lat1$date,0,4)
lat1<- lat1 %>%
  filter(doy >= 60) %>%
  filter(doy <= 181)

fx <- function(lat1, Tmean, n){
  warm <- rle(lat1$Tmean >0)
  gdd<-warm$lengths>= 7
  frz<-gdd$Tmin<=-2
}
  

# Subset Weather down for HF
weather<- weather %>%
  filter(Site == "hf") %>%
  filter(Year > 2000) %>%
  filter(JD > 60) %>%
  filter(JD < 181)

for(i in unique(budburst$year)){ # i = '2000'
  dxx <- budburst[budburst$year == i,]
  lox <- rank(dxx$l75.jd)
  lox[is.na(dxx$l75.jd)] = NA
  box <- rank(dxx$bb.jd, na.last = TRUE)
  box[is.na(dxx$bb.jd)] = NA
  lo = c(lo, lox)
  bb = c(bb, box)
  sp = c(sp, dxx$sp)
  yr = c(yr, rep(i, nrow(dxx)))
}

# rowSums(data > 0)
# apply(data>30,7,identity)
fx <- function(weather, AirT, n){
  warm <- rle(weather$AirT >0)
  gdd<-warm$lengths>= 7
}


for(i in 2001:2014)){
  dxx<-weather[weather$year == i,]
  dxx$warm<-rle(weather$AirT > 0)
  dxx$warm[is.na(dxx$AirT)] = NA
  dxx$frz<-ifelse(weather$AirTMin<0, Y, NA)
  yr = c(yr, rep(i, nrow(dxx)))
}

u1 <- rnorm(30)
usq<-0
for(i in 1:10) 
{
  usq[i]<-u1[i]*u1[i] # i-th element of u1 squared into i-th position of usq
  print(usq[i])
}
print(i)

weather$warm<- ifelse(weather$AirT>0, 1, 0)
weather<-na.omit(weather)
weather$inst<-length(weather$warm)["1"]
weather$inst<-length(weather)[weather$warm=="1"])
  
inst<- rle(as.data.frame(table(weather$warm)))
  warm<- rle(as.character(weather$AirT))
  dxx$inst<-tally(warm, sort = TRUE)

print(dxx)

> lapply(split(subRes[,2], subRes[,1]), function(x) {
  +             df.rle <- ifelse(x > 0, 1, 0)
  +             df.rle <- rle(df.rle)
  + 
    +             wh <- which(df.rle$lengths == max(df.rle$lengths))
    +             mx <- df.rle$lengths[wh]
    +             suma <- df.rle$lengths[1:wh]
    +             out <- x[(sum(suma) - (suma[length(suma)] - 1)):sum(suma)]
    +             return(out)
    +         })

z <- c(TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
rle(z)
rle(as.character(z))
print(rle(z), prefix = "..| ")
