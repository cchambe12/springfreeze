## Most updated script for Dan's experiment plots
# Cat - 17 April 2017

# Clear workspace
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
d<-read.csv("input/Budburst.clean.csv",header=TRUE)

########### NEW EDITION - CAT 17 APRIL 2017 ####################
tx<-c("CS0", "WL1")
dx<- d %>%
  dplyr::select(ind, treatcode, lday, bday, site) %>%
  filter(treatcode %in% tx)

dx<-na.omit(dx)
dx$species<-substr(dx$ind, 1, 6)
dx<-dx%>%filter(species!="VIBCAS")%>%filter(species!="VIBLAN") # all entries for two species have the same budburst and leafout day, removed because probably from error
df<-dx%>%unite(ID, species, treatcode, sep="_")

df$mean<-ave(df$bday, df$ID)
df$sd<-ave(df$bday, df$ID, FUN=sd)
df$mean.leaf<-ave(df$lday, df$ID)
df$sd.leaf<-ave(df$lday, df$ID, FUN=sd)  

df<-df%>%
  group_by(mean, ID)%>%
  arrange(ID)%>%
  filter(row_number()==1) 

ts.timeline<-ggplot((df), aes(x=bday, y=ID), stat="identity") + 
  geom_point(aes(x=df$bday, col="royalblue4")) +
  geom_point(aes(x=df$lday, col="forestgreen"))  + 
  xlab("Day of Year") +scale_color_manual(labels = c("Leafout","Budburst"), values = c("forestgreen","royalblue4")) +
  ylab("Species") +geom_errorbarh(aes(xmin=bday-sd, xmax=bday+sd, col="royalblue4"), height=.0) + 
  geom_errorbarh(aes(xmin=lday-sd.leaf, xmax=lday+sd.leaf, col="forestgreen"), height=.0)
plot(ts.timeline)

### Prep data for Anovas
dxx$DOY<-yday(dxx$Date)
dxx$chilling<- substr(dxx$chill, 6, 6)
#d$chilling<-as.numeric(as.character(
#ifelse((d$chilling==0), 0, ifelse((d$chilling==1), 4, 1.5))))
dxx$force<-as.numeric(as.character(ifelse((dxx$warm=="warm"), 20, 15)))
dxx$photoperiod<- as.numeric(as.character(ifelse((dxx$photo=="short"), 8, 12)))
dxx<-dxx %>%
  dplyr::select(id, sp, site, tleaf, DOY, chilling, force, photoperiod, treatcode)


# Run anovas for each species
myspp <- unique(d.hf$sp)
mylist<-list()
for(i in c(1:length(myspp))) {
  subby<-subset(d.hf, sp==myspp[i])
  myanova<-Anova(lm(risk~as.factor(chilling)+ force + photoperiod, data=subby))
  print(myanova)
  mylist[[myspp[i]]] <- myanova
}

# with all two way interactions
myspp <- unique(d.hf$sp)
mylist<-list()
for(i in c(1:length(myspp))) {
  subby<-subset(d.hf, sp==myspp[i])
  myanova<-Anova(lm(risk~as.factor(chilling)+ force + photoperiod + as.factor(chilling)*force +
                      as.factor(chilling)*photoperiod + force*photoperiod, data=subby))
  print(myanova)
  mylist[[myspp[i]]] <- as.data.frame(table(myanova))
}

