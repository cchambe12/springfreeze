## Most updated script for Dan's experiment plots - species differences
# Cat - 15 December 2017

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
dxx<-read.csv("output/diffplot.csv",header=TRUE)

species<- ggplot(dxx, aes(x=CS0, y=WL1)) + geom_point(aes(size=diff), shape=21) + 
  geom_linerange(aes(ymin=WL1-WL1_se, ymax=WL1+WL1_se), alpha=0.3) +
  geom_errorbarh(aes(xmax = CS0+CS0_se, xmin = CS0-CS0_se, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label=code), vjust=2, fontface="italic") + xlab("DVR with weak treatment effects") + 
  ylab("DVR with strong treatment effects") + 
  scale_size_continuous(name=expression(Delta*" in DVR"))
plot(species)


