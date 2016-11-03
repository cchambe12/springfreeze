# 3 NOVEMBER 2016 - CC
## Organizing Experimental data/individuals
## Renaming and grouping individuals by treatment

library(dplyr)
library(tidyr)
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# Set Working Directory
setwd("~/Documents/git/springfreeze/input")
experiment<-read.csv("Exp.Individs.csv",header=TRUE)
attach<- experiment

# Get the site by getting the last two characters of the undercomp rownames
experiment <- experiment %>%
  separate(ID, c("Individual", "Site"), by="_")

experiment$sp <- substr(experiment$sp, 0, 6) 

species<-as.data.frame(table(cleaner$Individual))
site<-as.data.frame(table(cleaner$Site))
