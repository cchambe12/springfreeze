# FALSE SPRING INDEX
# 24 May 2016
# Goal: compile weather data, phenology data, and NPN spring indices
#       to establish false spring indices for Common Garden Project
# Using: dply, tidyr, purrr
## When using 'select' function in dply, cannot have MASS package
## uploaded, interferes with dplyr package!!

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory

graphics.off()

# Install Packages
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
packages <- c("ggplot2", "rmarkdown", "dplyr", "knitcitations",
              "knitr","lattice", "tidyr")
ipak(packages)

# Integrate Phenology Data and compare methodologies
# Set Working Directory
setwd("~/Documents/git/falsespring")

budburst<-read.csv("hf003-06-mean-spp.csv",header=TRUE, sep=",")
attach(budburst)

d<- budburst %>%
  select(year, species, bb.jd) %>%
  filter(species == c("COAL",""))


obs_bb<- budburst %>%
  group_by(year)%>%
  summarise_each(funs(mean),bb.jd)%>%
  arrange(year)