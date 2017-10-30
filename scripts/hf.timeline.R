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
library(lme4)
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
w<-dplyr::rename(w, year=Year)
w<-dplyr::rename(w, doy=JD)
# Determine HF false springs based on data
w$ten<-ifelse((w$doy >=107 & w$doy <= 145 & w$year == 2010), w$count, 0)
w$four<-ifelse((w$doy >=130 & w$doy <= 158 & w$year == 2014), w$count, 0)
w$grow<-w$ten + w$four
w<-dplyr::select(w, -ten, -four)
w$grow<-ifelse((w$grow==0), NA, w$grow)
w<-na.omit(w)

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
df<-na.omit(df)

gdd<- ggplot((w), aes(doy, grow, col=factor(year))) + geom_line(aes(x=doy, y=grow, col=factor(year)), size=4) + 
  ylab("Growing Degree Days") + xlab("Day of Year") + labs(col="Year") 
plot(gdd)

# Make a graph!
df$code <- reorder(df$species, df$year)
df$ord<- reorder(df$code, df$bb.jd)


df$bb.jd<-as.numeric(as.character(df$bb.jd))
df$l75.jd<-as.numeric(as.character(df$l75.jd))

hf<-ggplot(df, aes(x=ord,ymin = bb.jd, ymax = l75.jd, group=interaction(species, year) )) +
  geom_point(aes(y=bb.jd, col="forestgreen"), position = position_dodge(.5)) + geom_point(aes(y=l75.jd, col="darkgreen"), position = position_dodge(.5)) +
  geom_linerange(aes(x=ord,ymin = bb.jd, ymax = l75.jd, col=factor(year)), position=position_dodge(.5)) +  ylab("Day of Year") +
  scale_color_manual(labels = c("2010", "2014", "Leafout", "Budburst"), values = c("#F8766D", "#00BFC4", "green4", "darkolivegreen3")) +
  xlab("Species") +coord_flip() + labs(color="Phenophase and Year")
plot(hf)



grid.newpage()
grid.draw(rbind(ggplotGrob(hf), ggplotGrob(gdd), size="first"))


df$mean<-ave(df$risk, df$year)
df$stand<-ave(df$risk, df$year, FUN = sd)

prep<-df%>%
  gather("phenophase", "doy", bb.jd, l75.jd)
grand_union<-full_join(w, prep)


## Analysis
dxx<-grand_union%>%
  dplyr::select(species, si, risk, count, phenophase, sp.year)
dxx<-na.omit(dxx)
dx<-spread(dxx, phenophase, count)
dx$gdd<-dx$l75.jd - dx$bb.jd
dx$year<-substr(dx$sp.year,6,9)

prep$phenophase<- ifelse(prep$phenophase=="bb.jd", "budburst", "leafout")
prep<-spread(prep, phenophase, doy)

prep$year<-as.numeric(prep$year)
dx$year<-as.numeric(dx$year)
d<-inner_join(prep, dx)

mod.gdd<-lmer(risk~gdd+as.factor(year)+(1|species), data=d)
arm::display(mod.gdd)

mod2<- lmer(risk~gdd + (1|year) + (1|species),data=dx)
arm::display(mod2)  
  
mod3<- lmer(risk~gdd*as.factor(year)+(1|species), data=dx)  
arm::display(mod3)    
  

class(d$year)

  

