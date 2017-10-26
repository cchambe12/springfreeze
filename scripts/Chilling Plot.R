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
library(car)
library(xtable)
library(broom)
library(tibble)
library(lme4)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
d<-read.csv("input/Budburst.clean.csv",header=TRUE)

########### NEW EDITION - CAT 26 May 2017 ####################
tx<-c("CS0", "WL1")
dx<- d %>%
  dplyr::select(ind, treatcode, lday, bday, site) %>%
  filter(treatcode %in% tx)

dx<-na.omit(dx)
dx$species<-substr(dx$ind, 1, 6)
dx<-dx%>%filter(species!="VIBCAS")%>%filter(species!="VIBLAN") # all entries for two species have the same budburst and leafout day, removed because probably from error
small.spp<-dx %>% dplyr::select(species, treatcode) %>% filter(treatcode=="WL1")
spp<-unique(small.spp$species)
dx<-dx%>% filter(species %in% spp)

dx<-dx%>%dplyr::select(-site, -ind)
dx$mean<-ave(dx$bday, dx$species, dx$treatcode)
dx<-dx%>%
  group_by(mean, species)%>%
  arrange(species)%>%
  filter(row_number()==1)
dx<-dx%>%group_by(species, treatcode) %>% arrange(species, desc(treatcode))
dx$code<-reorder(dx$species, dx$bday)

df<-dx%>%dplyr::select(species, treatcode, mean)
df<-spread(df, treatcode, mean)
df$diff<-as.numeric(df$CS0-df$WL1)
df$code<-reorder(df$species, df$WL1)

chill<-ggplot(dx, aes(x = code,ymin = bday, ymax = lday, group=interaction(species, treatcode) )) +
  geom_point(aes(y=bday, col="forestgreen"), position = position_dodge(.5)) + geom_point(aes(y=lday, col="darkgreen"), position = position_dodge(.5)) +
  geom_linerange(aes(x = code,ymin = bday, ymax = lday, col=treatcode), position=position_dodge(.5)) +  ylab("Day of Year") +
  scale_color_manual(labels = c("CS0","Leafout", "Budburst", "WL1"), values = c("purple3", "green4", "darkolivegreen3", "royalblue3")) +
  xlab("Species") +coord_flip()
plot(chill)

chill.small<-ggplot(df, aes(x = code,ymin = WL1, ymax = CS0, group=species)) +
  geom_point(aes(y=WL1, col="forestgreen"), position = position_dodge(.5)) + geom_point(aes(y=CS0, col="darkgreen"), position = position_dodge(.5)) +
  geom_linerange(aes(x = code,ymin = WL1, ymax = CS0), position=position_dodge(.5)) +  ylab("Difference in DVR between Treatments") +
  scale_color_manual(labels = c("DVR_CS0", "DVR_WL1"), values = c("purple3", "green4")) +
  xlab("Species") +coord_flip()
plot(chill.small)

dx$risk= dx$lday - dx$bday
risk<-ggplot(dx, aes(x=bday, y=risk)) + geom_point(aes(col=treatcode)) + geom_smooth(aes(x=bday, y=risk, col=treatcode, fill=treatcode), method= "loess")
plot(risk)


### Prep data for Anovas
dxx<-d
dxx$chilling<- as.numeric(as.character(substr(dxx$chill, 6, 6)))
#d$chilling<-as.numeric(as.character(
#ifelse((d$chilling==0), 0, ifelse((d$chilling==1), 4, 1.5))))
dxx$warm<-as.numeric(as.character(dxx$warm))
dxx$photo<-as.numeric(as.character(dxx$photo))
dxx$species<-substr(dxx$ind, 1, 6)
dxx<-dxx%>%filter(species!="VIBCAS")%>%filter(species!="VIBLAN") # all entries for two species have the same budburst and leafout day, removed because probably from error
small.spp<-dxx %>% dplyr::select(species, treatcode) %>% filter(treatcode=="WL1")
spp<-unique(small.spp$species)
dxx<-dxx%>% filter(species %in% spp)
dxx<-dxx %>%
  dplyr::select(id, species, site, lday, bday, chilling, warm, photo, treatcode)
dxx$risk<-dxx$lday-dxx$bday 

mod1<-lmer(risk~chilling + warm + photo + (chilling + warm + photo|species), data=dxx)
arm::display(mod1)

mod.inter<-lmer(risk~chilling + warm + photo + chilling*warm + 
                  chilling*photo + warm*photo + (chilling + warm + photo|species), data=dxx)
arm::display(mod.inter)

Anova(lm(risk~chilling + warm + photo, data=dxx))

# Run anovas for each species
myspp <- unique(dxx$species)
mylist<-list()
for(i in c(1:length(myspp))) {
  subby<-subset(dxx, species==myspp[i])
  myanova<-Anova(lm(risk~chilling + warm + photo, data=subby))
  print(myanova)
  mylist[[myspp[i]]] <- myanova
}

myspp <- unique(dxx$species)
mylist<-list()
for(i in c(1:length(myspp))) {
  subby<-subset(dxx, species==myspp[i])
  myanova<-Anova(lm(risk~as.factor(chilling)+ as.factor(warm) + as.factor(photo), data=subby))
  print(myanova)
  mylist[[myspp[i]]] <- myanova
}
nov1<-as_tibble(mylist)

# Still working on output! Trying to find better way to show results and fix results
novas<-as.data.frame(mylist, row.names = make.unique(rownames(mylist)))
#novas<-write.table(mylist, file="output/novas.csv")

#write.csv(mylist, "output/anovatable.csv", row.names=FALSE)
#xtableList(mylist, caption ="Anova results for Risk by chilling, forcing, and photoperiod effects for each species.", floating=FALSE)

Anova(lm(risk~as.factor(chilling)+ as.factor(warm) + as.factor(photo) + as.factor(chilling)*as.factor(warm) +
           as.factor(chilling)*as.factor(photo) + as.factor(warm)*as.factor(photo) data=dxx))

# with all two way interactions
myspps <- unique(dxx$species)
mylister<-list()
for(i in c(1:length(myspps))) {
  subbiest<-subset(dxx, species==myspps[i])
  menova<-Anova(lm(risk~as.factor(chilling)+ as.factor(warm) + as.factor(photo) + as.factor(chilling)*as.factor(warm) +
                     as.factor(chilling)*as.factor(photo) + as.factor(warm)*as.factor(photo), data=subbiest))
  print(menova)
  mylister[[myspps[i]]] <- menova
}

#write.csv(mylister, "output/interactions.csv", row.names=FALSE)
#write.csv(dxx, file="~/Documents/git/springfreeze/output/dvrdata_danf.csv", row.names=FALSE)
