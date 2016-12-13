## Making a Distance from Budburst to Leaf Out Script
# In Buds folder using Dan Flynn's data
# Clear workspace

## 30 November 2016 - Cat
## Attempt to create a timeline chart and eventally use Stan to determine
## the effects of latitude and temperature on risk time
## This is take two with a new datasheet from the USNPN, follows bb.leafout.R and darewearm.R
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

# Set Working Directory
setwd("~/Documents/git/springfreeze")
timeline<-read.csv("input/Budburst.DF.csv",header=TRUE)

# Convert to Julian day
timeline$DOY<-yday(timeline$Date)
phases<-c("4","7")
tx<-c("WL2","CS1", "WL0", "CS2") # may want to use WL1 instead?
timeline<-timeline %>%
  select(id, sp, site, tleaf, DOY, treatcode) %>%
  filter(treatcode %in% tx) %>%
  filter(tleaf %in% phases)
timeline$tleaf<- factor(timeline$tleaf, levels = c(4,7), 
       labels = c("Budburst","Leaves"))

hf<- timeline%>%
  filter(site == "HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
hf<-na.omit(hf)
hf$Risk <- hf$Leaves - hf$Budburst
hf<-filter(hf, Risk > 0)

hf.buds <- hf %>%
  filter(treatcode == "CS2") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

hf.leaves <- hf %>%
  filter(treatcode == "CS2") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

hf.Risk <- hf %>%
  filter(treatcode == "CS2") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

cold<-full_join(hf.buds, hf.leaves)
cold<-full_join(cold, hf.Risk)
cold$tx<-"CS2"

cs1.buds <- hf %>%
  filter(treatcode == "CS1") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

cs1.leaves <- hf %>%
  filter(treatcode == "CS1") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

cs1.Risk <- hf %>%
  filter(treatcode == "CS1") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

cs1<-full_join(cs1.buds, cs1.leaves)
cs1<-full_join(cs1, cs1.Risk)
cs1$tx<-"CS1"

warm.buds <- hf %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

warm.leaves <- hf %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

warm.Risk <- hf %>%
  filter(treatcode == "WL2") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

warm<-full_join(warm.buds, warm.leaves)
warm<-full_join(warm, warm.Risk)
warm$tx<-"WL2"

med.buds <- hf %>%
  filter(treatcode == "WL1") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

med.leaves <- hf %>%
  filter(treatcode == "WL1") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

med.Risk <- hf %>%
  filter(treatcode == "WL1") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

med<-full_join(med.buds, med.leaves)
med<-full_join(med, med.Risk)
med$tx<-"WL1"

wlo.buds <- hf %>%
  filter(treatcode == "WL0") %>%
  select(sp,id,Budburst) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Budburst)

wlo.leaves <- hf %>%
  filter(treatcode == "WL0") %>%
  select(sp,id,Leaves) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Leaves)

wlo.Risk <- hf %>%
  filter(treatcode == "WL0") %>%
  select(sp,id,Risk) %>%
  group_by(sp) %>%
  summarise_each(funs(mean), Risk)

wlo<-full_join(wlo.buds, wlo.leaves)
wlo<-full_join(wlo, wlo.Risk)
wlo$tx<-"WL0"

hf<-bind_rows(warm,cold, med, wlo, cs1)
species<-c("ACERUB", "BETPAP", "ILEMUC", "POPGRA", "QUERUB")
hf<- hf %>%
  group_by(sp,tx) %>%
  filter(sp %in% species)
sp.code<-hf %>%
  unite(sp.code, sp, tx)
sp.code$tx<- substr(sp.code$sp.code, 8, 10)

# Make a plot
sp.code$code <- reorder(sp.code$sp.code, sp.code$Leaves)

hf.plot<-ggplot((sp.code), aes(x=Budburst, y=code), stat="identity") + geom_point(aes(x= hf$Budburst)) + 
  geom_segment(aes(y = sp.code, yend = sp.code, x = Budburst, xend = Leaves, col=tx)) +
  geom_point(aes(x=Leaves, col=tx)) + geom_point(aes(col=tx)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")
plot(hf.plot)

ggplot((sp.code), aes( x= Budburst, y=Risk)) + geom_smooth(method="lm", se=FALSE) + geom_point(aes(col=tx))


# Summary Data by Treatment
cs0<- sp.code %>%
  filter(tx=="CS0")
cs0$sd<-sd(cs0$Risk)
cs0$mean<-mean(cs0$Risk)

cs1<- sp.code %>%
  filter(tx=="CS1")
cs1$sd<-sd(cs1$Risk)
cs1$mean<-mean(cs1$Risk)

cs2<- sp.code %>%
  filter(tx=="CS2")
cs2$sd<-sd(cs2$Risk)
cs2$mean<-mean(cs2$Risk)

wl0<- sp.code %>%
  filter(tx=="WL0")
wl0$sd<-sd(wl0$Risk)
wl0$mean<-mean(wl0$Risk)

wl1<- sp.code %>%
  filter(tx=="WL1")
wl1$sd<-sd(wl1$Risk)
wl1$mean<-mean(wl1$Risk)

wl2<- sp.code %>%
  filter(tx=="WL2")
wl2$sd<-sd(wl2$Risk)
wl2$mean<-mean(wl2$Risk)
