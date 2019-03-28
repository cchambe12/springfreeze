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

# Set Working Directory
setwd("~/Documents/git/springfreeze")
d<-read.csv("input/Budburst.clean.csv",header=TRUE)

########### NEW EDITION - CAT 26 May 2017 ####################
tx<-c("CS0", "WL1")

### Prep data for Anovas
dxx<-d
dxx$species<-substr(dxx$ind, 1, 6)
dxx<-dxx%>%filter(species!="VIBCAS")%>%filter(species!="VIBLAN") # all entries for two species have the same budburst and leafout day, removed because probably from error
small.spp<-dxx %>% dplyr::select(species, treatcode) %>% filter(treatcode=="WL1")
spp<-unique(small.spp$species)
dxx<-dxx%>% filter(species %in% spp)
dxx$chill<- as.numeric(as.character(substr(dxx$chill, 6, 6)))

dxx<-dplyr::select(dxx, id, sp, site, ind, warm, photo, chill, lday, bday)
dxx<-dxx[!is.na(dxx$lday),]
dxx<-dxx[!is.na(dxx$bday),]
dxx$risk<-dxx$lday-dxx$bday 
dxx<-dxx[(dxx$risk>0),]
#write.csv(dxx, file="~/Documents/git/springfreeze/output/expdata_dvr.csv", row.names = FALSE)

