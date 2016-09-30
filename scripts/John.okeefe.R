# FALSE SPRING INDEX
# 15 September 2016 - Started by Cat
# Goal: compile weather data, phenology data, and NPN spring indices
#       to establish false spring indices
# In this script, I am working to sort through John O'Keefe's dataset to 
# make a more appropriate comparison to the NPN data.

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
packages <- c("ggplot2", "dplyr", "lattice", "tidyr")
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

# Compares FSI values for small dataset of Dr O'Keefe's observational
# data and USNPN Spring Indices, based on -1.7 deg Celcius last freeze
method<-read.csv("method.test.csv",header=TRUE,sep=",")
attach(method)

bb.table<-method %>%
  select(year,last_frz,bb_npn, sm.bb) %>%
  filter(year>=2008)%>%
  filter(year<2015)%>%
  rename("Last Freeze"=last_frz)%>%
  rename("Observed"=sm.bb)%>%
  rename("SI-x"=bb_npn)

FSI.table<- method %>%
  select(year, FSI_npn, FSI_sm, FSI_obs, FSI_cam) %>%
  filter(year>=2008) %>%
  filter(year<2015)

blend<-FSI.table %>% 
  gather(Methodologies, FSI, -year) %>%
  arrange(year)

methodplot<-ggplot(blend, aes(year, FSI)) + xlab("Year") +
  ylab("False Spring Index") +
  geom_point(aes(col=Methodologies))
plot(methodplot)

npn_sm <- method %>%
  select(year, FSI_npn, FSI_sm, FSI_obs, FSI_cam) %>%
  filter(year>=2008) %>%
  filter(year<2015)
regression <- glm(formula = FSI_npn ~ FSI_sm + year, 
                  data = npn_sm)
summary(regression)

pearson<-cor(FSI.table, method="pearson")
pearson