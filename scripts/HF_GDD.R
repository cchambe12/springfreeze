## 8 March 2017
# GDD Script and Budburst for HF
# Aim: To find the average accumulated growing degree days for Harvard Forest till budburst
# using John O'Keefe's data

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
hf<- read.csv("input/WeatherData.csv", header=TRUE)
d<-read.csv("input/method.test.csv",header=TRUE)
budburst<-read.csv("input/hf003-06-mean-spp.csv",header=TRUE, sep=",")

# Use budburst
sp <- budburst %>%
  dplyr::select(year, species, bb.jd) %>%
  arrange(bb.jd) %>%
  group_by(species, year)%>%
  arrange(year) 

df<-as.data.frame(table(budburst$species))

spp<- c("PRSE", "AMSP", "POTR", "CRSP", "HAVI", "ACSA", "BEPA", "ACPE", "ACRU", "QURU", 
        "BEAL", "BELE") ## Species that were observed each year

sp.short<- c("ACPE","ACRU","ACSA","FAGR","FRAM","HAVI","QUAL",
             "QURU","PRSE", "AMSP", "POTR", "CRSP","BEPA",
             "BEAL", "BELE", "COAL", "QUVE")

d <- budburst %>%
  dplyr::select(year, species, bb.jd)%>%
  filter(species %in% sp.short)

df<- d %>%
  dplyr::select(species, bb.jd) %>%
  arrange(bb.jd)

obs_bb.avg<- d %>%
  dplyr::select(year,bb.jd)%>%
  group_by(year)%>%
  summarise_each(funs(mean), bb.jd) %>%
  rename(mean=bb.jd)%>%
  arrange(year)

obs_bb.sd<- d %>%
  group_by(year)%>%
  summarise_each(funs(sd), bb.jd) %>%
  rename(sd=bb.jd)%>%
  arrange(year)

obs_bb<- obs_bb.avg%>%
  right_join(obs_bb.sd,obs_bb.avg,by="year")%>%
  arrange(year)

# Use l75.jd to find leaf out
d.leaf <- budburst %>%
  dplyr::select(year, species, l75.jd)%>%
  filter(species %in% sp.short)

df.leaf<- d.leaf %>%
  dplyr::select(species, l75.jd) %>%
  arrange(l75.jd)

leaf.avg<- d.leaf %>%
  dplyr::select(year,l75.jd)%>%
  group_by(year)%>%
  summarise_each(funs(mean), l75.jd) %>%
  rename(mean=l75.jd)%>%
  arrange(year)

leaf.sd<- d.leaf %>%
  group_by(year)%>%
  summarise_each(funs(sd), l75.jd) %>%
  rename(sd=l75.jd)%>%
  arrange(year)

leaf<- leaf.avg%>%
  right_join(leaf.sd,leaf.avg,by="year")%>%
  arrange(year)

# Harvard Forest
# To double check my script is accurate
hf<- filter(hf, Year>=2009)
hf<- filter(hf, Site == "hf")
hf$gdd <- hf$AirT - 5
hf$gdd <-ifelse(hf$gdd>0, hf$gdd, 0)
hf$count <- ave(
  hf$gdd, hf$Year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)

# Clean up dataframes and join
hf<-rename(hf, year=Year)
d<-obs_bb%>%
  dplyr::select(year, mean)
d<- as.data.frame(rapply(object = d, f = round, classes = "numeric", how = "replace", digits = 0)) 
df<-left_join(hf, d)
df<-df%>%
  dplyr::select(year, JD, mean, count) %>%
  filter(year>=2009)
df$doy<-ifelse(df$JD==df$mean, df$JD, NA)
df<-na.omit(df)
mean<-mean(df$count)

leaf.df<-leaf%>%
  dplyr::select(year, mean)
leaf.df<- as.data.frame(rapply(object = leaf.df, f = round, classes = "numeric", how = "replace", digits = 0)) 
df2<-left_join(hf, leaf.df)
df2<-df2%>%
  dplyr::select(year, JD, mean, count) %>%
  filter(year>=2009)
df2$doy<-ifelse(df2$JD==df2$mean, df2$JD, NA)
df2<-na.omit(df2)
mean2<-mean(df2$count)

# Determine HF false springs based on data
hf$frz<- ifelse((hf$AirTMin<=-4), "freeze", "thaw")
hf$fs<- ifelse((hf$count >= 100 & hf$frz == "freeze" & hf$count<=400), TRUE, NA)

hf.count<- dplyr::select(hf, year, fs)
hf.count<-na.omit(hf.count)
hf.count<-as.data.frame(table(hf.count$year))

## Make a curve with GDD for 2010 and 2014
years<-c(2010,2014)
hf<-hf%>%filter(year %in%years)
hf<-hf %>%
  filter(JD >= 107) %>%
  filter(JD <= 158)

ggplot((hf), aes(JD, count, col=factor(year))) + geom_line(aes(x=JD, y=count, col=factor(year)), alpha=0.2, size=4 )
