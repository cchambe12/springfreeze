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
tbl_df(budburst)
glimpse(budburst)
obs_bb<- budburst %>%
  group_by(year)%>%
  summarise_each(funs(mean),bb.jd)%>%
  arrange(year)



# Compares FSI values for Phenology Cam, Dr O'Keefe's observational
# data and USNPN Spring Indices, based on -1.7 deg Celcius last freeze
method<-read.csv("method.test.csv",header=TRUE,sep=",")
attach(method)

bb.table<-method %>%
  select(Year,last_frz,bb_obs,bb_cam,bb_npn) %>%
  filter(Year>=2008)%>%
  filter(Year<2015)%>%
  rename("Last Freeze"=last_frz)%>%
  rename("Observed"=bb_obs)%>%
  rename("PhenoCam"=bb_cam)%>%
  rename("SI-x"=bb_npn)

species<-as.data.frame(table(budburst$species))

FSI.table<- method %>%
  select(Year, FSI_obs, FSI_cam, FSI_npn) %>%
  filter(Year>=2008) %>%
  filter(Year<2015)

blend<-FSI.table %>% 
  gather(Methodologies, FSI, -Year) %>%
  arrange(Year)

methodplot<-ggplot(blend, aes(Year, FSI)) + xlab("Year") +
  ylab("False Spring Index") +
  geom_point(aes(col=Methodologies))
plot(methodplot)

npn_cam <- method %>%
  select(Year, FSI_npn, FSI_cam) %>%
  filter(Year>=2008) %>%
  filter(Year<2015)
regression <- glm(formula = FSI_npn ~ FSI_cam + Year, 
                  data = npn_cam)
summary(regression)
plot(regression)

npn_obs <- method %>%
  select(Year, FSI_npn, FSI_obs) %>%
  filter(Year>=2008) %>%
  filter(Year<2015)
regression.npn <- glm(formula = FSI_npn ~ FSI_obs + Year, 
                   data = npn_obs)
summary(regression.npn)
plot(regression.npn)

# Pearson Correlation of all three methodologies
pearson<-cor(FSI.table, method="pearson")
pearson
# Compare FSI data from observational data and Phenology Cam
obs.hf<- method %>%
  select(Year, FSI_obs, FSI_cam) %>%
  filter(Year>=2008) %>%
  filter(Year<2015)

blend.hf<-obs.hf %>% 
  gather(Methodologies, FSI, -Year) %>%
  arrange(Year)

regression.hf <- glm(formula = FSI_obs ~ FSI_cam + Year, 
                   data = obs.hf)
summary(regression.hf)
plot(regression.hf)

hf<-ggplot(blend.hf, aes(Year, FSI)) + xlab("Year") +
  ylab("False Spring Index") +
  geom_point(aes(col=Methodologies)) + geom_smooth(method="loess")

# Plot FSI Phenology Camera Data for -1.7 deg Celcius
false_spring <- FSI.table %>%
  select(Year,FSI_cam) %>%
  arrange(Year)
index<-pmax(FSI_cam,0)

ggplot(false_spring, aes(Year,FSI_cam)) +
  xlab("Year") + ylab("False Spring Index") +
  geom_point()

ggplot(false_spring, aes(Year,index)) +
  xlab("Year") + ylab("False Spring Index") +
  geom_point()

hf.sh<- method %>%
  select(FSI_cam, FSI_sh, Year) %>%
  rename("hf" = FSI_cam) %>%
  rename("sh" = FSI_sh) %>%
  filter( Year >= 2014) %>%
  filter(Year< 2016) %>%
  gather(Site, FSI, -Year) %>%
  arrange(Year)

gradient<- ggplot(hf.sh, aes(Year, FSI)) + geom_point(aes(col=Site)) + 
  xlab("Year") + ylab("FSI") + geom_smooth(method="lm") + 
  scale_x_continuous(limits = c(2014,2015),breaks=seq(2014,2015,1))
