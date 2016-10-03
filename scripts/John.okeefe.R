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
packages <- c("ggplot2", "dplyr", "lattice", "tidyr", "broom")
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


boxplot(data=blend, FSI~Method, xlab="Method", ylab="FSI")
boxplot(data=blend.long, FSI~Method)

methodplot<-ggplot(blend, aes(year, FSI)) + xlab("Year") +
  ylab("False Spring Index") +
  geom_point(aes(col=Method)) + geom_smooth()
plot(methodplot)

longplot<-ggplot(blend.long,aes(year,FSI)) +xlab("Year") +
  ylab("False Spring Index") + scale_x_continuous(limits = c(2001,2014),breaks=seq(2001,2014,1)) +
  geom_point(aes(col=Method)) + geom_smooth()
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

install.packages("xtable")
library(xtable)
pears<-xtable(pearson, caption="Pearson Correlation Coefficients shown comparing the strength of association between the FSI values calculated across all three methodologies.")
write.csv(pears, file ="cortable.csv")
long<-xtable(pearson.long, caption="Pearson Correlation Coefficients shown comparing the strength of association between the FSI values calculated across all three methodologies.")
write.csv(long, file ="cor.long.csv")

## Attempting Bayesian Correlation
library(BayesFactor)

jzs_corbf=function(r,n){
  int=function(r,n,g){
    (1+g)^((n-2)/2)*(1+(1-r^2)*g)^(-(n-1)/2)*
      g^(-3/2)*exp(-n/(2*g))};
  bfl0=sqrt((n/2))/gamma(1/2)*integrate(int,lower=0,upper=Inf,r=r,n=n)$value; 
  return(bfl0)}
r <- 0.9497443
n <- 5
jzs_corbf(r,n)

##Bayes Factor ttest - not sure how to interpret results...
npn.sm<-method%>%
  select(year,FSI_npn,FSI_sm)%>%
  filter(year>=2001)%>%
  filter(year<2015)%>%
  gather(Method, FSI, -year) %>%
  arrange(year)
npn.obs<-method%>%
  select(year,FSI_npn,FSI_obs)%>%
  filter(year>=2001)%>%
  filter(year<2015)%>%
  gather(Method, FSI, -year) %>%
  arrange(year)
npn.cam<-FSI.table%>%
  select(year,FSI_npn,FSI_cam)%>%
  gather(Method, FSI, -year) %>%
  arrange(year)
sm.obs<-method%>%
  select(year,FSI_sm,FSI_obs)%>%
  filter(year>=2001)%>%
  filter(year<2015)%>%
  gather(Method, FSI, -year) %>%
  arrange(year)
sm.cam<-FSI.table%>%
  select(year,FSI_sm,FSI_cam)%>%
  gather(Method, FSI, -year) %>%
  arrange(year)
obs.cam<-FSI.table%>%
  select(year,FSI_obs,FSI_cam)%>%
  gather(Method, FSI, -year) %>%
  arrange(year)

t.test(FSI~Method, data=npn.sm)
ttestBF(formula=FSI~Method, data=npn.sm)
t.test(FSI~Method, data=npn.obs)
ttestBF(formula=FSI~Method, data=npn.obs)
t.test(FSI~Method, data=npn.cam)
ttestBF(formula=FSI~Method, data=npn.cam)
t.test(FSI~Method, data=sm.obs)
ttestBF(formula=FSI~Method, data=sm.obs)
t.test(FSI~Method, data=sm.cam)
ttestBF(formula=FSI~Method, data=sm.cam)
t.test(FSI~Method, data=obs.cam)
ttestBF(formula=FSI~Method, data=obs.cam)
