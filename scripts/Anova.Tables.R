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
d.betall <- d[,21:24]
d.betpap <- d[,29:32]
d.faggra <- d[,37:40]
d.ilemuc <- d[,49:52]
d.popgra <- d[,69:72]
d.querub <- d[,81:84]


