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
d<-read.csv("output/anovatable.csv",header=TRUE)
dxx<-read.csv("output/interactions.csv",header=TRUE)

d.acepen <- d[,2:5]
d.acerub<- d[,6:9]
d.acesac <- d[,10:13]
d.betall <- d[,14:17]
d.betpap <- d[,18:21]
d.faggra <- d[,22:25]
d.ilemuc <- d[,26:39]
d.popgra <- d[,30:33]
d.querub <- d[,34:37]

dxx.acepen <- dxx[,1:4]
dxx.acerub<- dxx[,5:8]
dxx.acesac <- dxx[,9:12]
dxx.betall <- dxx[,13:16]
dxx.betpap <- dxx[,17:20]
dxx.faggra <- dxx[,21:24]
dxx.ilemuc <- dxx[,25:28]
dxx.popgra <- dxx[,29:32]
dxx.querub <- dxx[,33:36]