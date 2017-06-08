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
library(car)
# Set Working Directory
setwd("~/Documents/git/springfreeze")
timeline<-read.csv("input/hf003-06-mean-spp.csv", header=TRUE)
weather<-read.csv("input/WeatherData.csv", header=TRUE)

# Sort Weather Data
years<- c(2010, 2014)

w<-weather %>%
  filter(Year %in% years) %>%
  filter(JD<=160)
w$gdd <- w$AirT - 0
w$gdd <-ifelse(w$gdd>0, w$gdd, 0)
w$count <- ave(
  w$gdd, w$Year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
w<-rename(w, year=Year)
w<-rename(w, doy=JD)
# Determine HF false springs based on data
w$frz<- ifelse((w$AirTMin<=-4), "freeze", "thaw")
w$fs<- ifelse((w$count >= 100 & w$frz == "freeze" & w$count<=400), TRUE, NA)

w.count<- dplyr::select(w, year, fs)
w.count<-na.omit(w.count)
w.count<-as.data.frame(table(w.count$year))

climate<-ggplot((w), aes(x=JD, y=AirT)) + xlab("Day of Year") + ylab("Mean Daily Temperature (C)") +
  geom_point(aes(col=factor(Year)))+
  geom_line(aes(x=JD, y=AirT, col=factor(Year))) + scale_shape_manual(values=c("#999999", "#56B4E9"),
                                                                     name="Year") +
  scale_color_manual(labels = c("2010", "2014"), values = c("purple3", "royalblue3")) +
  labs(color="Year")
climate

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
w<-w%>%filter(doy>=100)
gdd<- ggplot((w), aes(doy, count, col=factor(year))) + geom_line(aes(x=doy, y=count, col=factor(year)), size=4) + ylab("Growing Degree Days") + xlab("Day of Year") + labs(col="Year")
airT<-ggplot((w), aes(doy, AirT, col=factor(year))) + ylab("Average Daily Air Temperature (C)") + xlab("Day of Year") + labs(col="Year") +
  geom_smooth(aes(col=factor(year)), method="loess")
# Make a graph!
df$code <- reorder(df$species, df$bb.jd)

df$bb.jd<-as.numeric(as.character(df$bb.jd))
df$l75.jd<-as.numeric(as.character(df$l75.jd))

hf<-ggplot(df, aes(x = code,ymin = bb.jd, ymax = l75.jd, group=interaction(species, year) )) +
  geom_point(aes(y=bb.jd, col="forestgreen"), position = position_dodge(.5)) + geom_point(aes(y=l75.jd, col="darkgreen"), position = position_dodge(.5)) +
  geom_linerange(aes(x = code,ymin = bb.jd, ymax = l75.jd, col=factor(year)), position=position_dodge(.5)) +  ylab("Day of Year") +
  scale_color_manual(labels = c("2010", "2014", "Leafout", "Budburst"), values = c("#F8766D", "#00BFC4", "green4", "darkolivegreen3")) +
  xlab("Species") +coord_flip() + labs(color="Phenophase and Year", title="Harvard Forest Data")
plot(hf)

grid.newpage()
grid.draw(rbind(ggplotGrob(hf), ggplotGrob(gdd), size="first"))


prep<-df%>%
  gather("phenophase", "doy", bb.jd, l75.jd)
grand_union<-full_join(w, prep)
grand<-grand_union%>%
  dplyr::select(year, doy, count, species, phenophase, risk, code, AirT)




overlay<-ggplot(grand) +
  geom_point(aes(y=bb.jd, col="forestgreen", x=code), position = position_dodge(.5)) + geom_point(aes(y=l75.jd, col="darkgreen", x=code), position = position_dodge(.5)) +
  geom_linerange(aes(x = code,ymin = bb.jd, ymax = l75.jd, col=factor(year)), position=position_dodge(.5)) +  xlab("Day of Year") +
  ylab("Species") + labs(color="Phenophase and Year", title="Harvard Forest Data") + geom_line(aes(y=doy, x=count, col=factor(year)), alpha=0.2, size=4) + coord_flip()
overlay


## Analysis
mod.gdd<-glm(risk~count, data=grand)
display(mod.gdd)

mod.airt<-lmer(risk~AirT + (1|year), data=grand)
display(mod.airt)
grand$myear<-ave(grand$risk, grand$year)

