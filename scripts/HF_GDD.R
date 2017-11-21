# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(rstanarm)
library(arm)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
timeline<-read.csv("input/hf003-06-mean-spp.csv", header=TRUE)
weather<-read.csv("input/WeatherData.csv", header=TRUE)

hf<-timeline[!is.na(timeline$l75.jd),]

spp<-as.data.frame(table(hf$species))
spp$include<-ifelse(spp$Freq==25, TRUE, NA)
spp<-spp[!is.na(spp$include),]
sp<-unique(spp$Var1)
d<-timeline%>%
  dplyr::select(year, species, bb.jd, l75.jd) %>%
  filter(species%in%sp)
d$year<-as.numeric(d$year)
d$risk<-d$l75.jd-d$bb.jd
d<-na.omit(d)

df<-d%>%
  gather("phenophase", "doy", bb.jd, l75.jd)
x<-paste(df$year, df$doy)
df$Date<-as.Date(strptime(x, format="%Y %j"))

## Weather data
w<-weather %>%
  filter(Site == "hf")%>%
  filter(JD<=180)
w$AirT<-ifelse(is.na(w$AirT), 0, w$AirT)
w$gdd <- w$AirT - 5
w$gdd <-ifelse(w$gdd>0, w$gdd, 0)
w$count <- ave(
  w$gdd, w$Year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
w<-dplyr::rename(w, year=Year)
w<-dplyr::rename(w, doy=JD)
x<-paste(w$year, w$doy)
w$Date<-as.Date(strptime(x, format="%Y %j"))
w<-w[!is.na(w$AirTMin),]
w$frz<-ifelse(w$AirTMin<0, 1, 0)
w$freezes<-ave(
  w$frz, w$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
#w<-dplyr::select(w, Date, gdd, count, frz)
#w$year<-as.numeric(substr(w$Date, 1, 4))


dxx<-d
dxx$fs.lo<-NA
for(i in c(1:nrow(w))){
  for(j in c(1:nrow(dxx)))
      dxx$fs.lo[j]<-ifelse(dxx$l75.jd[j]==w$doy[i] & dxx$year[j]==w$year[i], w$freezes[i], dxx$fs.lo[j])
}
dxx$fs.bb<-NA
for(i in c(1:nrow(w))){
  for(j in c(1:nrow(dxx)))
    dxx$fs.bb[j]<-ifelse(dxx$bb.jd[j]==w$doy[i] & dxx$year[j]==w$year[i], w$freezes[i], dxx$fs.bb[j])
}
dxx$afrzs<-dxx$fs.lo-dxx$fs.bb
dxx$risk<-dxx$l75.jd-dxx$bb.jd


dx<-inner_join(df,w)
count<-dplyr::select(dx, doy, year, count)
count<-count[!duplicated(count),]
count$doy<-as.numeric(count$doy)
count<-filter(count, count<=500)

gdd<-d
gdd$bb.jd<-as.numeric(gdd$bb.jd)
gdd$bb.gdd<-NA
gdd$l75.gdd<-NA
for(i in c(1:nrow(gdd))){
  for(j in c(1:nrow(count)))
    gdd$bb.gdd[i]<-ifelse(gdd$bb.jd[i]==count$doy[j] & gdd$year[i]==count$year[j], count$count[j], gdd$bb.gdd[i])  
}
for(i in c(1:nrow(gdd))){
  for(j in c(1:nrow(count)))
    gdd$l75.gdd[i]<-ifelse(gdd$l75.jd[i]==count$doy[j] & gdd$year[i]==count$year[j], count$count[j], gdd$l75.gdd[i])  
}
gdd$agdd<-gdd$l75.gdd-gdd$bb.gdd

gx<-full_join(dxx, gdd)
years<-c("2010", "2014")
gx<-filter(gx, year%in%years)
gx$z.agdd<-scale(gx$agdd, center=TRUE, scale=FALSE)
gx$z.year<-scale(gx$year, center=TRUE, scale=FALSE)
gx$z.bb<-scale(gx$bb.jd, center=TRUE, scale=FALSE)
m1<-lm(risk~afrzs+z.year+z.agdd, data=gx)
display(m1)
m2<-lm(l75.gdd~bb.jd+afrzs, data=gx)
display(m2)
m3<-lm(bb.jd~afrzs+agdd, data=gx)
display(m3)
m4<-lm(risk~agdd+afrzs,data=gx)
display(m4)


fit1<-stan_glm(risk~bb.gdd+bb.jd, data=gx)
fit1
plot(fit1, pars="beta")


w$bb<-NA
for(i in c(1:nrow(w))){
  for(j in c(1:nrow(d)))
    w$bb[i]<-ifelse(w$doy[i]==d$bb.jd[j] & w$year[i]==d$year[i], d$species[j], w$bb[i])
}
w$lo<-NA
for(i in c(1:nrow(w))){
  for(j in c(1:nrow(d)))
    w$lo[i]<-ifelse(w$doy[i]==d$l75.jd[j] & w$year[i]==d$year[i], d$species[j], w$lo[i])
}
fs<-full_join(w, df, by=c("doy", "year", "Date"))
fs<-dplyr::select(fs, Date, year, doy, count, phenophase, frz, species)
fs<-fs%>%
  filter(year>=1990)%>%
  filter(doy>=90)

fs$sp.year<-paste(fs$species, fs$year)
for(i in unique(fs$sp.year)){
  fs$freezes <- ave(
    fs$frz, fs$species, fs$year, 
    FUN=function(x) cumsum(c(0, head(x, -1)))
  )
}


gdd$z.agdd<-scale(gdd$agdd, center=TRUE, scale=FALSE)
gdd$z.bb<-scale(gdd$bb.jd, center=TRUE, scale=FALSE)
gdd$z.year<-scale(gdd$year, center=TRUE, scale=FALSE)
gdd$z.risk<-scale(gdd$risk, center=TRUE, scale=FALSE)
gdd$z.bbgdd<-scale(gdd$bb.gdd, center=TRUE, scale=FALSE)
gdd$mean<-ave(gdd$bb.jd, gdd$year)
gdd$z.mean<-scale(gdd$mean, center=TRUE, scale=FALSE)
gdd$m.risk<-ave(gdd$risk, gdd$year)

dg<-dplyr::select(gdd, -z.agdd, -z.bb, -z.year, -z.risk, -z.bbgdd)
dg$mean<-ave(dg$bb.jd, dg$year)

mod1<-stan_glm(risk~agdd+mean, data=gdd)
mod1
plot(mod1, pars="beta")

avg<-dplyr::select()

gdd$year<-as.numeric(gdd$year)
m1<-lm(m.risk~year, data=gdd)
display(m1)

m2<-lm(risk~bb.jd, data=gdd)
display(m2)

m3<-lm(agdd~risk+bb.jd, data=gdd)
display(m3)


years<-c("1996", "2012")

ggplot(gdd, x=species, y=bb.gdd) + geom_boxplot(aes(x=species, y=bb.gdd))


gdd$mean<-ave(gdd$bb.gdd, gdd$year)



w.yr<-w%>%
  filter(year%in% years)%>%
  dplyr::select(year, doy, AirTMin)%>%
  filter(doy>=110)%>%
  filter(doy<=155)
w.yr$fs<-ifelse(w.yr$AirTMin<=-2, w.yr$AirTMin, NA)
w.yr<-w.yr[!is.na(w.yr$fs),]


gdd$ord<- reorder(gdd$species, gdd$risk)
gdd.yr$code <- reorder(gdd.yr$species, gdd.yr$year)
gdd.yr$ord<- reorder(gdd.yr$code, gdd.yr$bb.jd)
gdd.yr<-filter(gdd, year%in%years)

gdd.yr$m.risk<-ave(gdd.yr$risk, gdd.yr$year)

hf<-ggplot(gdd.yr, aes(x=ord,ymin = bb.gdd, ymax = l75.gdd, group=interaction(species, year) )) +
  geom_point(aes(y=bb.gdd, col="forestgreen"), position = position_dodge(.5)) + geom_point(aes(y=l75.gdd, col="darkgreen"), position = position_dodge(.5)) +
  geom_linerange(aes(x=ord,ymin = bb.gdd, ymax = l75.gdd, col=factor(year)), position=position_dodge(.5)) +  ylab("GDDs")  +
  scale_color_manual(labels = c("1996", "2012", "Leafout GDD", "Budburst GDD"), 
                       values = c("#F8766D", "#00BFC4", "green4", "darkolivegreen3")) + 
  xlab("Species") +coord_flip() + labs(color="Phenophase and Year") + geom_hline(yintercept=c(84, 121), color="#F8766D",
                                                                                 linetype=2) +
  geom_hline(yintercept=196, color="#00BFC4", linetype=2)
plot(hf)

hf.bb<-ggplot(gdd.yr, aes(x=ord,ymin = bb.jd, ymax = l75.jd, group=interaction(species, year) )) +
  geom_point(aes(y=bb.jd, col="forestgreen"), position = position_dodge(.5)) + geom_point(aes(y=l75.jd, col="darkgreen"), position = position_dodge(.5)) +
  geom_linerange(aes(x=ord,ymin = bb.jd, ymax = l75.jd, col=factor(year)), position=position_dodge(.5)) +  ylab("Day of Year") +
  scale_color_manual(labels = c("1996", "2012", "Leafout", "Budburst"), values = c("#F8766D", "#00BFC4", "green4", "darkolivegreen3")) +
  xlab("Species") +coord_flip() + labs(color="Phenophase and Year")
plot(hf.bb)


gdd.yr$z.agdd<-scale(gdd.yr$agdd, center=TRUE, scale=FALSE)
m1<-stan_glm(risk~agdd+bb.jd+as.factor(year), data=gdd.yr)
m1
plot(m1, pars="beta")
