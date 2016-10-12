## Downloaded USNPN Data to Establish Timeline from Bud Burst
## to Leaf Out for various Deciduous Broadleaf species around the US
## 12 October 2016 - Cat

# The core aim of this script is to establish a timeline for last spring freeze risk

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/input/springfreeze")
phenology<-read.csv("individual_phenometrics_data.csv",header=TRUE, sep=",")
attach(phenology)


