## Tables ready for anova
# Cat - 21 April 2017

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
d<-read.csv("output/anovatable.csv",header=TRUE, colClasses="numeric")


d.acepen <- d[,1:4]
d.acerub<- d[,5:8]
d.acesac <- d[,9:12]
d.alninc <- d[,13:16]
d.aromel <- d[,17:20]
d.betall <- d[,21:24]
d.betlen <- d[,25:28]
d.betpap <- d[,29:32]
d.corcor <- d[,33:36]
d.faggra <- d[,37:40]
d.franig <- d[,41:44]
d.hamvir <- d[,45:48]
d.ilemuc <- d[,49:52]
d.kalang <- d[,53:56]
d.loncan <- d[,57:60]
d.lyolig <- d[,61:64]
d.nyssyl <- d[,65:68]
d.popgra <- d[,69:72]
d.prupen <- d[,73:76]
d.quealb <- d[,77:80]
d.querub <- d[,81:84]
d.quevel <- d[,85:88]
d.rhafra <- d[,89:92]
d.rhopri <- d[,93:96]
d.spialb <- d[,97:100]
d.vacmyr <- d[,101:104]

