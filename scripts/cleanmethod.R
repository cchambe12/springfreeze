#Cleaning method.test.csv file 
## 14 November 2016 - Cat
# Want to integrate all methods of determining budburst for the data since they are all significantly correlated

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/springfreeze/input")
d<-read.csv("method.test.csv",header=TRUE)
attach(d)

# Cleaning
spring<- d %>%
  select(year, bb_npn, bb_obs, bb_cam, bb_sm, FSI_npn, FSI_obs, FSI_cam, FSI_sm) %>%
  unite(BB, bb_npn, bb_obs, bb_cam, bb_sm, -year, -FSI_npn, -FSI_obs, -FSI_cam, -FSI_sm) %>%
  unite(FSI, FSI_npn, FSI_obs, FSI_cam, bb_sm, -year, -FSI_npn, -FSI_obs, -FSI_cam, -FSI_sm) 