rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
library(data.table)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
d<-read.csv("input/Budburst.DF.csv",header=TRUE)

d$DOY<-yday(d$Date)
d$chilling<- substr(d$chill, 6, 6)
#d$chilling<-as.numeric(as.character(
#ifelse((d$chilling==0), 0, ifelse((d$chilling==1), 4, 1.5))))
d$force<-as.numeric(as.character(ifelse((d$warm=="warm"), 20, 15)))
d$photoperiod<- as.numeric(as.character(ifelse((d$photo=="short"), 8, 12)))
phases<-c("4","7")
d<-d %>%
  dplyr::select(id, sp, site, tleaf, DOY, chilling, force, photoperiod, treatcode) %>%
  filter(tleaf %in% phases)
d$tleaf<- factor(d$tleaf, levels = c(4,7), 
                 labels = c("Budburst","Leaves"))

## Harvard Forest Data
d.hf<-d%>%
  filter(site=="HF") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.hf$risk<-d.hf$Leaves-d.hf$Budburst 
d.hf<-filter(d.hf,risk>0)
d.hf<-na.omit(d.hf)

d.total<-as.data.frame(table(d.hf$sp)) %>%
  rename(sp=Var1)%>%
  rename(total=Freq)
d.total$total<-ifelse(d.total$total>=3, d.total$total, NA)
d.total<-na.omit(d.total)
spp<-as.character(d.total$sp)
d.hf<-filter(d.hf, sp %in% spp)

# keep condensing species list -- error from chilling, only one chilling tx for some species
d.sp<-d.hf %>%
  ungroup(d.hf)%>%
  dplyr::select(sp, chilling)
d.sp<-unique(d.sp)
d.sp<-as.data.frame(table(d.sp$sp)) %>%
  rename(sp=Var1)%>%
  rename(total=Freq)
d.sp$total<-ifelse(d.sp$total>1, d.sp$total, NA)
d.sp<-na.omit(d.sp)
species<-as.character(d.sp$sp)
d.hf<-filter(d.hf, sp %in% species)

df<-d.hf%>%
  ungroup(d.hf)%>%
  dplyr::select(-site, -chilling, -force, -id, -photoperiod)%>%
  unite(ID, sp, treatcode, sep="_")

df$mean<-ave(df$Budburst, df$ID)
df$sd<-ave(df$Budburst, df$ID, FUN=sd)
df$mean.leaf<-ave(df$Leaves, df$ID)
df$sd.leaf<-ave(df$Leaves, df$ID, FUN=sd)

y<- df %>%
  dplyr::select(ID, Budburst, Leaves, risk, mean, sd, mean.leaf, sd.leaf) %>%
  gather(Phenophase, DOY, -ID, -risk, -mean, -sd, -mean.leaf, -sd.leaf)
y<-y %>%
  group_by(ID, mean, Phenophase)%>%
  arrange(ID)%>%
  filter(row_number()==1)%>%
  ungroup(y)

y3 = melt(setDT(y), 
          measure.vars = patterns("mean", "mean.leaf"), 
          variable.name = "Mean")
y3[, Mean := factor(Mean, labels = c("bud","leaf"))]
y3<-rename(y3, Budburst=value1, Leaves=value2)
y3<- y3 %>%
  dplyr::select(-Mean)%>%
  arrange(Phenophase)%>%
  gather(pheno, Mean, -ID, -risk, -Phenophase, -DOY, -sd, -sd.leaf)
y3$Phenophase<- ifelse(y3$Phenophase==y3$pheno, y3$Phenophase, NA)
y3<-na.omit(y3)
y3<- dplyr::select(y3, -pheno)

y2 = melt(setDT(y3), 
          measure.vars = patterns("sd", "sd.leaf"), 
          variable.name = "sd")
y2[, sd := factor(sd, labels = c("bud","leaf"))]
y2<-rename(y2, Budburst=value1, Leaves=value2)
y2<- y2 %>%
  dplyr::select(-sd)%>%
  arrange(Phenophase)%>%
  gather(pheno, sd, -ID, -risk, -Phenophase, -DOY, -Mean)
y2$Phenophase<- ifelse(y2$Phenophase==y2$pheno, y2$Phenophase, NA)
y2<-na.omit(y2)
y2<- dplyr::select(y2, -pheno)
y2$code <- reorder(y2$ID, y2$DOY)

dan<-ggplot((y2), aes(x=code, y=DOY)) + geom_point(aes(col=Phenophase)) + 
  xlab("Day of Year") + ylab("Species") + geom_errorbar(aes(ymin=DOY-sd, ymax=DOY+sd, col=Phenophase), width=.0) +
  scale_x_discrete() +
  theme(legend.position="none")
plot(dan)

## Other Plot ##
bud<- df %>%
  dplyr::select(ID, Budburst) %>%
  group_by(ID)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(ID)

leaves<- df %>%
  dplyr::select(ID, Leaves) %>%
  group_by(ID)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(ID)

sd<- df %>%
  dplyr::select(ID, sd, sd.leaf)

basic<- full_join(bud, leaves)
basic$Risk<- basic$Leaves - basic$Budburst
basic<-full_join(basic, sd)

basic$code <- substr(basic$ID, 8,10)
basic<-arrange(basic, code)
basic$sd<-ifelse(basic$sd>0, basic$sd, NA)
basic$sd.leaf<-ifelse(basic$sd.leaf>0, basic$sd.leaf, NA)
basic<-na.omit(basic)

ts.timeline<-ggplot((basic), aes(x=Budburst, y=ID), stat="identity") + 
  geom_point(aes(x=basic$Budburst, col="coral")) +
  geom_point(aes(x=basic$Leaves, col="forestgreen")) + theme(legend.position="none") + 
    xlab("Budburst to Leaf Out") +
  ylab("Species") +geom_errorbarh(aes(xmin=Budburst-sd, xmax=Budburst+sd, col="coral"), height=.0) + 
  geom_errorbarh(aes(xmin=Leaves-sd.leaf, xmax=Leaves+sd.leaf, col="forestgreen"), height=.0)
plot(ts.timeline)
