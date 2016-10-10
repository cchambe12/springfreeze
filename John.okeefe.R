# FALSE SPRING INDEX
# 15 September 2016 - Started by Cat
# Goal: compile weather data, phenology data, and NPN spring indices
#       to establish false spring indices
# In this script, I am working to sort through John O'Keefe's dataset to 
# make a more appropriate comparison to the NPN data.

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
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
setwd("~/Documents/git/springfreeze")

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

obs_bb.avg<- d %>%
  select(year,bb.jd)%>%
  group_by(year)%>%
  summarise_each(funs(mean), bb.jd) %>%
  rename(mean=bb.jd)%>%
  arrange(year)

obs_bb.sd<- d %>%
  group_by(year)%>%
  summarise_each(funs(sd), bb.jd) %>%
  rename(sd=bb.jd)%>%
  arrange(year)

obs_bb<- obs_bb.avg%>%
  right_join(obs_bb.sd,obs_bb.avg,by="year")%>%
  arrange(year)
  

summarise_each(funs(mean),bb.jd)%>%

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
FSI.long<- method%>%
  select(year,FSI_npn,FSI_sm, FSI_obs)%>%
  filter(year>=2001)%>%
  filter(year<2015)
blend<-FSI.table %>% 
  gather(Method, FSI, -year) %>%
  arrange(year)
blend.long<-FSI.long%>%
  gather(Method,FSI,-year)%>%
  arrange(year)

ggplot(blend, (aes(Method, FSI)), xlab="Method", ylab="FSI") + 
  geom_boxplot(fill=c("#F8766D","#00C094","#00B6EB","#A58AFF"))

boxplot(data=blend, FSI~Method, xlab="Method", ylab="FSI")
boxplot(data=blend.long, FSI~Method)

methodplot<-ggplot(blend, aes(year, FSI)) + xlab("Year") +
  ylab("False Spring Index") + scale_x_continuous(limits = c(2008,2014),breaks=seq(2008,2014,1)) +
  geom_point(aes(col=Method)) + scale_color_manual(values=c("#F8766D","#00C094","#00B6EB","#A58AFF"))
plot(methodplot)

longplot<-ggplot(blend.long,aes(year,FSI)) +xlab("Year") +
  ylab("False Spring Index") + scale_x_continuous(limits = c(2001,2014),breaks=seq(2001,2014,1)) +
  geom_point(aes(col=Method)) + scale_color_manual(values=c("#00C094","#00B6EB","#A58AFF"))
plot(longplot)

fit<-glm(FSI~Method + year, data=blend)
fit
tidy(fit)
head(augment(fit))
glance(fit)
plot(fit)

npn_sm <- method %>%
  select(year, FSI_npn, FSI_sm, FSI_obs, FSI_cam) %>%
  filter(year>=2008) %>%
  filter(year<2015)
regression <- glm(formula = FSI_npn ~ FSI_sm + year, 
                  data = npn_sm)
summary(regression)

pearson<-cor(FSI.table, method="pearson")
pearson
tidy(pearson)
glance(pearson)
pearson.long<-cor(FSI.long, method="pearson")
pearson.long
