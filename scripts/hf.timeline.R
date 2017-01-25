## Making a Distance from Budburst to Leaf Out Script
# Using HF data for one unusually early and one unusually late spring up date

## 13 Dec 2016- Cat
## Attempt to create a timeline chart
# Using John O'Keefe's Data!!

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(arm)
library(grid)
# Set Working Directory
setwd("~/Documents/git/springfreeze")
timeline<-read.csv("input/hf003-06-mean-spp.csv", header=TRUE)
weather<-read.csv("input/WeatherData.csv", header=TRUE)

# Sort Weather Data
years<- c(2010, 2014)

w<-weather %>%
  filter(Year %in% years) %>%
  filter(JD >= 100) %>%
  filter(JD <= 160)

climate<-ggplot((w), aes(x=JD, y=AirT)) + xlab("Day of Year") + ylab("Mean Daily Temperature (C)") +
  geom_point(aes(col=factor(Year)))+
  geom_line(aes(x=JD, y=AirT, col=factor(Year))) + scale_shape_manual(values=c("#999999", "#56B4E9"),
                                                                     name="Year")
climate

grid.newpage()
grid.draw(rbind(ggplotGrob(df.plot), ggplotGrob(climate), size = "last"))

# Add Risk and only Two Years
years<-c("2010", "2014")
timeline<-timeline %>%
  dplyr::select(year, species, bb.jd, l75.jd) %>%
  filter(year%in%years)
timeline<-na.omit(timeline)
timeline$risk <- timeline$l75.jd - timeline$bb.jd
df<- timeline %>%
  unite(sp.year, species, year, remove=FALSE)
df$si[df$year=="2010"] <- "early"
df$si[df$year=="2014"] <- "late"
#df<-na.omit(df)

# Make a graph!
#df$code <- reorder(df$sp.year, df$bb.jd)

df.plot<-ggplot((df), aes(x=bb.jd, y=sp.year), stat="identity") + geom_point(aes(x= df$bb.jd)) + 
  geom_segment(aes(y = sp.year, yend = sp.year, x = bb.jd, xend = l75.jd, col=si)) +
  geom_point(aes(x=l75.jd, col=si)) + geom_point(aes(col=si)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")
plot(df.plot)

ggplot((df), aes( x=bb.jd, y=risk)) + geom_smooth(method="lm", se=FALSE) + geom_point(aes(col=si))

lmodel<-lm(risk~bb.jd + year,data=df)
display(lmodel)

# Summarize Data
early<- df%>%
  filter(si=="early")
summary(early$risk)
early$sd<-sd(early$risk)
early$mean<-mean(early$risk)
early.lm<-lm(early$bb.jd~early$risk)
summary(early.lm)
plot(early$bb.jd, early$risk)
late<- df%>%
  filter(si=="late")
late.lm<-lm(late$risk~late$bb.jd)
summary(late.lm)
plot(late$bb.jd,late$risk)
summary(late$risk)
late$sd<-sd(late$risk)
late$mean<-mean(late$risk)

# Integrate weather data and risk data
risk.climate<- df %>%
  dplyr::select(year, bb.jd, l75.jd, risk, species, sp.year, si) %>%
  gather(phenophase, JD, -year, -risk, -species, -sp.year, -si) 

risk.climate.df<-full_join(w, risk.climate, by="JD") %>%
  dplyr::select(JD,AirT,year,risk,species,phenophase, sp.year, si)
risk.na<-na.omit(risk.climate.df)

querub<-risk.climate.df %>%
  filter(species=="QURU")

mod<-lm(risk~AirT, data = risk.climate.df)
summary(mod)

## Attempt to make overlapping graph
risk.plot<- ggplot((risk.climate.df), aes(x=JD, y=sp.year), stat="identity") + geom_point(aes(x= df$bb.jd)) + 
  geom_segment(aes(y = sp.year, yend = sp.year, x = bb.jd, xend = l75.jd, col=si)) +
  geom_point(aes(x=l75.jd, col=si)) + geom_point(aes(col=si)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")
weather.plot<- ggplot((risk.climate.df), aes(x=JD, AirT))
grid.newpage()
grid.draw(rbind(ggplotGrob(risk.plot), ggplotGrob(weather.plot), size = "last"))
