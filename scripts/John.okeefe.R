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
setwd("~/Documents/git/springfreeze/input")

budburst<-read.csv("hf003-06-mean-spp.csv",header=TRUE, sep=",")
attach(budburst)

sp <- budburst %>%
  select(year, species, bb.jd) %>%
  arrange(bb.jd) %>%
  group_by(species, year)%>%
  arrange(year) 

spp<- c("PRSE", "AMSP", "POTR", "CRSP", "HAVI", "ACSA", "BEPA", "ACPE", "ACRU", "QURU", 
                         "BEAL", "BELE") ## Species that were observed each year

d <- budburst %>%
  select(year, species, bb.jd)%>%
  filter(species %in% spp)

df<- d %>%
  select(species, bb.jd) %>%
  arrange(bb.jd)

obs_bb<- d %>%
  group_by(year)%>%
  summarise_each(funs(mean),bb.jd)%>%
  arrange(year)

compare<- read.csv("method.test.csv", header=TRUE, sep=",")
attach(compare)

obs.npn<- obs_bb %>%
  full_join(compare) %>%
  select(year, bb.jd, bb_npn) %>%
  arrange(year) %>%
  filter(year>=1990) %>%
  filter(year<2015)

## Species that are more comparable to lilacs and honeysuckle ##
sm.sp<- c("PRSE", "POTR", "AMSP", "CRSP", "HAVI", "ACSA", "BEPA", "ACPE")
small<- budburst%>%
  select(year, species, bb.jd) %>%
  filter(species %in% sm.sp)
condensed<- small %>%
  group_by(year)%>%
  summarise_each(funs(mean),bb.jd)%>%
  arrange(year)

sm.comp<- condensed %>%
  full_join(compare) %>%
  select(year, bb.jd, bb_npn, last_frz) %>%
  arrange(year) %>%
  filter(year>=1990) %>%
  filter(year<2015)

