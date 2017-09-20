## 19 September 2017 - Cat
#     1. Is budburst delayed and require more GDDs after a false spring year? 
# Look at John O’Keefe data and gdd information to determine bb to lo at Harvard Forest

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

# Integrate Phenology Data and compare methodologies
# Set Working Directory
setwd("~/Documents/git/springfreeze/input")

bb<-read.csv("hf003-06-mean-spp.csv",header=TRUE, sep=",")
d<-read.csv("~/Documents/git/freezingexperiment/analyses/input/weather_allsites.csv", header=TRUE)

all.years<-as.data.frame(table(bb$species))
all.years$Freq<-ifelse(all.years$Freq==25, 25, NA)
keep<-na.omit(all.years)
spp<-unique(keep$Var1)


bb<-dplyr::select(bb, year, species, bb.jd, l75.jd)
bb<-filter(bb, species %in% spp)

bx<- gather(bb, key = phase, value = doy,
       bb.jd, l75.jd)


d<-dplyr::rename(d, doy=Julian.Date)
d$year<-substr(d$Date, 7, 10)
hf<-d%>%filter(site=="hf")%>%filter(year>=1990)
hf<-hf %>% filter(doy <=240)
## Quick Fix for weather data...
hf<-hf[!duplicated(hf[c("year","doy")]),]
## GDD calculations...
# Clean missing weather data...
hf<-hf[!is.na(hf$AirTMax),]
hf$gdd <- hf$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
hf$gdd <-ifelse(hf$gdd>0, hf$gdd, 0)
hf$count <- ave(
  hf$gdd, hf$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

hf<-dplyr::select(hf, year, doy, count)
hf$year<-as.numeric(hf$year)

bx.bb<-subset(bx, phase=="bb.jd")
hf$species<-NA
bx.bb$year<-as.numeric(bx.bb$year)
#for(i in c(1:nrow(hf))) {
#  for(j in c(1:nrow(bx.bb)))
#    if(hf$year[i] == bx.bb$year[j] & hf$doy[i] == bx.bb$doy[j]){
#      hf$species[i]<-bx.bb$species[j]
#    }
#      else{
#        hf$species[i]<-hf$species[i]
#      }
#}

for(i in c(1:nrow(hf))) {
  for(j in c(1:nrow(bx.bb)))
    hf$species[i]<-ifelse(hf$year[i] == bx.bb$year[j] & hf$doy[i] == bx.bb$doy[j], bx.bb$species[j],
                          hf$species[i])
}

hf<-na.omit(hf)

########################## Looking at Individual level too ###########################
bi<-read.csv("hf003-05-mean-ind.csv", header=TRUE)

bi<-filter(bi, species %in% spp)

bi$ind<-substr(bi$tree.id, 6, 7)
bi$ind<-as.numeric(bi$ind)

## Double check data...
yr.ind<-bi %>% dplyr::select(year, species, ind)
yr.ind$sp.yr<-paste(yr.ind$year, yr.ind$species)
yr.ind<-aggregate(yr.ind$ind, by = list(yr.ind$sp.yr), max)
d.ind<-yr.ind
d.ind$year<-as.numeric(substr(d.ind$Group.1, 0,4))
d.ind$species<-substr(d.ind$Group.1, 6, 9)
d.ind$ind<-as.numeric(d.ind$x)
d.ind<-d.ind%>%dplyr::select(year, species, ind)

bi$sp.ind<-paste(bi$species, bi$ind, sep="_")
bi<-dplyr::select(bi, year, species, bb.jd, l75.jd, ind, sp.ind)
bi<- gather(bi, key = phase, value = doy,
            bb.jd, l75.jd)

d<-dplyr::rename(d, doy=Julian.Date)
d$year<-substr(d$Date, 7, 10)
hf<-d%>%filter(site=="hf")%>%filter(year>=1990)
hf<-hf %>% filter(doy <=240)
## Quick Fix for weather data...
hf<-hf[!duplicated(hf[c("year","doy")]),]
## GDD calculations...
# Clean missing weather data...
hf<-hf[!is.na(hf$AirTMax),]
hf$gdd <- hf$AirTMax - 5 # Can be 0 here if want 0 degC as threshold
hf$gdd <-ifelse(hf$gdd>0, hf$gdd, 0)
hf$count <- ave(
  hf$gdd, hf$year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

hf<-dplyr::select(hf, year, doy, count)
hf$year<-as.numeric(hf$year)

bi.ind<-subset(bi, phase=="bb.jd")
bi.ind$count<-NA
for(i in c(1:nrow(bi.ind))) {
  for(j in c(1:nrow(hf)))
    bi.ind$count[i]<-ifelse(bi.ind$year[i] == hf$year[j] & bi.ind$doy[i] == hf$doy[j], hf$count[j],
                          bi.ind$count[i])
}

mod<-lm(count~year + species, data=bi.ind)
mod<-lmer(count~species + (1|year), data=bi.ind)
display(mod)
ggplot(bi.ind, aes(x=year, y=count)) + geom_point(aes(color=species)) + 
  stat_summary(aes(y = count,group=1), fun.y=mean, colour="red", geom="line",group=1) +
  stat_smooth(method="lm", se=FALSE)
ggplot(bi.ind, aes(x=year, y=gdd.year)) + geom_point()

hf<-na.omit(hf)







dx<-full_join(hf, bb)
dx<-dx%>%dplyr::select(year, species, bb.jd, l75.jd)
dx$risk<-dx$l75.jd-dx$bb.jd

df.bb<-subset(df, phase=="bb.jd")
df.bb$gdd.year<-ave(df.bb$count, df.bb$year)

check<-dplyr::select(df.bb, gdd.year, year)
check<-check[!(duplicated(check)),]

ggplot(df.bb, aes(x=year, y=count)) + geom_point(aes(color=species))
ggplot(df.bb, aes(x=year, y=gdd.year)) + geom_point()


##### Try again
fs<-read.csv("~/Documents/git/freezingexperiment/analyses/output/hf_fsi.csv", header=TRUE)
hf<-hf[(!is.na(hf$species)),]
hf$last.frz<-NA
for(i in c(1:nrow(hf))) {
  for(j in c(1:nrow(fs)))
    if(hf$year[i] == fs$year[j])
      hf$last.frz[i]<-fs$last[j]
}

