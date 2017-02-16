## Running Anova's for DanF's data
# In Buds folder using Dan Flynn's data
## LEARN HOW TO LOOP FOR EACH SPECIES AND CALCULATING RISK!!!! ##

## 10 Feb 2017 - Cat
# Using Dan's Data!!

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
library(car)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
#install.packages(packageurl, repos=NULL, type="source")

# Set Working Directory
setwd("~/Documents/git/springfreeze")
d<-read.csv("input/Budburst.DF.csv",header=TRUE)

d$DOY<-yday(d$Date)
d$chilling<- as.numeric(as.character(substr(d$chill, 6, 6)))
d$force<-as.numeric(as.character(ifelse((d$warm=="warm"), 20, 15)))
d$photoperiod<- as.numeric(as.character(ifelse((d$photo=="short"), 8, 12)))
phases<-c("4","7")
d<-d %>%
  dplyr::select(id, sp, site, tleaf, DOY, chilling, force, photoperiod, treatcode) %>%
  filter(tleaf %in% phases)
d$tleaf<- factor(d$tleaf, levels = c(4,7), 
                        labels = c("Budburst","Leaves"))

d.sp<-d%>%
  filter(site=="HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.sp$risk<-d.sp$Leaves-d.sp$Budburst 
d.sp<-filter(d.sp,risk>0)

spp<-unique(d.sp$sp)
for (i in 1:length(spp)){
  mod<-aov(risk~sp[i]+chilling+ force+photoperiod, data=d.sp)
}
summary.lm(mod)
for(i in 1:length(unique(d.sp$sp))){
  d.sp<-filter(d.sp, sp==i)
  species<-aov(risk[i]~chilling[i] + force[i] + photoperiod[i], data=d.sp)
}

+ (chilling*force) + 
  (chilling*photoperiod) + (force*photoperiod),data=each)
print(species)
species<-as.data.frame(table(d$sp))

#ACEPEN
d.ace<- d%>%
  filter(sp=="ACEPEN") %>%
  filter(site=="HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.ace$Risk <- d.ace$Leaves - d.ace$Budburst
d.ace<-filter(d.ace, Risk > 0)

acepen<-aov(Risk~chilling + force + photoperiod + (chilling*force) + 
             (chilling*photoperiod) + (force*photoperiod), data=d.ace)
acepen.tx<-aov(Risk~treatcode, data=d.ace)
summary.lm(acepen)
summary.lm(acepen.tx)

# ACERUB
for(i in 1:length(unique(d$sp))){
  d.sp<-filter(sp==sp[i]) %>%
    filter(site=="HF") %>%
    group_by(sp, id, tleaf)%>%
    arrange(id)%>%
    filter(row_number()==1) %>%
    spread(tleaf, DOY)
  d.sp$Risk <- d.sp$Leaves - d.sp$Budburst
  d.sp<-filter(d.sp, Risk > 0)
  print(i)
}

d.sp<-d%>%
  filter(site=="HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.sp$risk<-d.sp$Leaves-d.sp$Budburst 
d.sp<-filter(d.sp,Risk>0)
mod<-aov(Risk~sp+chilling+force+photoperiod, data=d.sp)
d.rub<- d%>%
  filter(sp=="ACERUB") %>%
  filter(site=="HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.rub$Risk <- d.rub$Leaves - d.rub$Budburst
d.rub<-filter(d.rub, Risk > 0)

acerub<-aov(Risk~chilling + force + photoperiod + (chilling*force) + 
              (chilling*photoperiod) + (force*photoperiod), data=d.rub)
acerub.tx<-aov(Risk~treatcode, data=d.rub)
summary.lm(acerub)
summary.lm(acerub.tx)
#LONCAN - not enough data
# POPGRA
d.pop<- d%>%
  filter(sp=="POPGRA") %>%
  filter(site=="HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.pop$Risk <- d.pop$Leaves - d.pop$Budburst
d.pop<-filter(d.pop, Risk > 0)

popgra<-aov(Risk~chilling + force + photoperiod + (chilling*force) + 
              (chilling*photoperiod) + (force*photoperiod), data=d.pop)
popgra.tx<-aov(Risk~treatcode, data=d.pop)
summary.lm(popgra)
summary.lm(popgra.tx)

# PRUPEN
d.pru<- d%>%
  filter(sp=="PRUPEN") %>%
  filter(site=="HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.pru$Risk <- d.pru$Leaves - d.pru$Budburst
d.pru<-filter(d.pru, Risk > 0)

prupen<-aov(Risk~chilling + force + photoperiod + (chilling*force) + 
              (chilling*photoperiod) + (force*photoperiod), data=d.pru)
prupen.tx<-aov(Risk~treatcode, data=d.pru)
summary.lm(prupen)
summary.lm(prupen.tx)