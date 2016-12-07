## Making a Distance from Budburst to Leaf Out Script
# In Buds folder using Dan Flynn's data
# Clear workspace

## 30 November 2016 - Cat
## Attempt to create a timeline chart and eventally use Stan to determine
## the effects of latitude and temperature on risk time
## This is take two with a new datasheet from the USNPN, follows bb.leafout.R and darewearm.R
# Using Dan's Data!!

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
hf<-read.csv("output/DF.timeline.csv",header=TRUE)

# Make the plot
hf.plot<-ggplot((hf), aes(x=Budburst, y=sp)) + geom_point(aes(x= hf$Budburst)) + 
  geom_segment(aes(y = sp, yend = sp, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=hf$Leaves)) + geom_point(aes(col=tx)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")
plot(hf.plot)

