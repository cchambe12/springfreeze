# 7 April 2017 - Cat
## Make a new script based off NA_west, Eur50, etc.
# Instead of using GDDs as guidance, attempt to find probability of low 
# temperatures within a timeframe where budburst is likely for a region
# Break into 2 week periods and then make boxplots
## Help from Ben Cook!

# Currently by Week, maybe convert to monthly instead?


# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(gtable)
library(reshape2)
library(grid)
library(egg)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
lat<-read.csv("input/NOAA_Eur50.csv", header=TRUE)
amer<-read.csv("input/NOAA_data2.csv", header=TRUE)
nc<-read.csv("input/N.Carolina.csv", header=TRUE)
new<-read.csv("input/Newport.csv", header=TRUE)
mid<-read.csv("input/midwest.csv", header=TRUE)
france<-read.csv("input/Lyon.csv", header=TRUE)

#####################################################################
## Plots by Month
# Bamberg,Germany: DOY 90-120
lat1<-lat %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BAMBERG GM") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
lat1$year <- substr(lat1$date, 0, 4)
lat1<- lat1 %>%
  filter(year>=1965) %>%
  filter(year<2016)
lat1$month<- substr(lat1$date, 5, 6)
lat1$day<- substr(lat1$date, 7,8)
lat1<- lat1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
lat1$doy<-yday(lat1$date)
lat1$day<-substr(lat1$date,9,10)
lat1$year<-substr(lat1$date,0,4)
lat1$month<-substr(lat1$date, 6,7)
lat1$biweek<-ifelse(lat1$day<=15,1,2)
lat1<-lat1%>%unite(biweekly,month,biweek, sep="_")
lat1$Tmean <- (lat1$Tmax + lat1$Tmin)/2
lat1$gdd <- lat1$Tmean - 5
lat1$gdd <-ifelse(lat1$gdd>0, lat1$gdd, 0)
lat1$frz<- ifelse((lat1$Tmin<=-2.2), 1, 0)
lat1$count <- ave(
  lat1$frz, lat1$biweekly, lat1$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
lat1<- lat1 %>%
  filter(doy >= 90) %>%
  filter(doy <= 120)

gm.count<- lat1 %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
gm.count$mean<-ave(gm.count$count, gm.count$biweekly)
gm.count$stand_dev<-ave(gm.count$count, gm.count$biweekly, FUN=sd)
gm.high<-gm.count%>%group_by(biweekly)%>%summarise(high=max(count))%>%ungroup(gm.count)
gm.low<-gm.count%>%group_by(biweekly)%>%summarise(low=min(count))%>%ungroup(gm.count)
gm.count<-full_join(gm.count, gm.high)
gm.count<-full_join(gm.count, gm.low)
gm<-gm.count %>%
  ungroup(gm.count) %>%
  dplyr::select(-count, -year) 
gm<-gm%>% filter (! duplicated(biweekly))
gm$site<-"Bavaria, DE"
gm.count$site<-"Bavaria, DE"

# Waterville, ME: DOY 100-150
water<-mid %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "WATERVILLE WWTP ME US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
water$year <- substr(water$date, 0, 4)
water<- water %>%
  filter(year>=1965) %>%
  filter(year<2016)
water$month<- substr(water$date, 5, 6)
water$day<- substr(water$date, 7,8)
water<- water %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
water$doy<-yday(water$date)
water$year<-substr(water$date,0,4)
water$month<-substr(water$date, 6,7)
water$day<-substr(water$date, 9,10)
water$biweek<-ifelse(water$day<=15,1,2)
water<-water%>%unite(biweekly,month,biweek, sep="_")
water$Tmean <- (water$Tmax + water$Tmin)/2
water$gdd <- water$Tmean - 5
water$gdd <-ifelse(water$gdd>0, water$gdd, 0)
water$frz<- ifelse((water$Tmin<=-2.2), 1, 0)
water$count <- ave(
  water$frz, water$biweekly, water$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
water<- water %>%
  filter(doy >= 100) %>%
  filter(doy <= 150)

water.count<- water %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
water.count$mean<-ave(water.count$count, water.count$biweekly)
water.count$stand_dev<-ave(water.count$count, water.count$biweekly, FUN=sd)
water.high<-water.count%>%group_by(biweekly)%>%summarise(high=max(count))%>%ungroup(water.count)
water.low<-water.count%>%group_by(biweekly)%>%summarise(low=min(count))%>%ungroup(water.count)
water.count<-full_join(water.count, water.high)
water.count<-full_join(water.count, water.low)
maine<-water.count %>%
  ungroup(water.count) %>%
  dplyr::select(-count, -year) 
maine<-maine%>% filter (! duplicated(biweekly))
maine$site<-"Maine, USA"
water.count$site<-"Maine, USA"

# Yakima Airport: March 22-Apr 30
am8<-amer %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "YAKIMA AIRPORT WA US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
am8$year <- substr(am8$date, 0, 4)
am8<- am8 %>% 
  filter(year>=1965) %>%
  filter(year<2016)
am8$month<- substr(am8$date, 5, 6)
am8$day<- substr(am8$date, 7,8)
am8<- am8 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
am8$doy<-yday(am8$date)
am8$year<-substr(am8$date,0,4)
am8$month<-substr(am8$date, 6,7)
am8$day<-substr(am8$date, 9,10)
am8$biweek<-ifelse(am8$day<=15,1,2)
am8<-am8%>%unite(biweekly,month,biweek, sep="_")
am8$Tmean <- (am8$Tmax + am8$Tmin)/2
am8$gdd <- am8$Tmean - 5
am8$gdd <-ifelse(am8$gdd>0, am8$gdd, 0)
am8$frz<- ifelse((am8$Tmin<=-2.2), 1, 0)
am8$count <- ave(
  am8$frz, am8$biweekly, am8$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
am8<- am8 %>%
  filter(doy >= 80) %>%
  filter(doy <= 120)

wash.count<- am8 %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
wash.count$mean<-ave(wash.count$count, wash.count$biweekly)
wash.count$stand_dev<-ave(wash.count$count, wash.count$biweekly, FUN=sd)
wash.high<-wash.count%>%group_by(biweekly)%>%summarise(high=max(count))%>%ungroup(wash.count)
wash.low<-wash.count%>%group_by(biweekly)%>%summarise(low=min(count))%>%ungroup(wash.count)
wash.count<-full_join(wash.count, wash.high)
wash.count<-full_join(wash.count, wash.low)
wash<-wash.count %>%
  ungroup(wash.count) %>%
  dplyr::select(-count, -year) 
wash<-wash%>% filter (! duplicated(biweekly))
wash$site<-"Washington, USA"
wash.count$site<-"Washington, USA"

# North Carolina: Feb 21 - Apr 4
## 1967 and 2012 both have one extra count (-9999degC)
nc1<-nc %>%
  dplyr::select(STATION_NAME,DATE, TMIN, TMAX) %>%
  filter(STATION_NAME == "REIDSVILLE 2 NW NC US") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
nc1$year <- substr(nc1$date, 0, 4)
nc1<- nc1 %>%
  filter(year>=1965) %>%
  filter(year<2016)
nc1$month<- substr(nc1$date, 5, 6)
nc1$day<- substr(nc1$date, 7,8)
nc1<- nc1 %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date, Tmin, Tmax)
nc1$doy<-yday(nc1$date)
nc1$year<-substr(nc1$date,0,4)
nc1$month<-substr(nc1$date, 6,7)
nc1$day<-substr(nc1$date,9,10)
nc1$biweek<-ifelse(nc1$day<=15,1,2)
nc1<-nc1%>%unite(biweekly,month,biweek, sep="_")
nc1$Tmean <- (nc1$Tmax + nc1$Tmin)/2
nc1$gdd <- nc1$Tmean - 5
nc1$gdd <-ifelse(nc1$gdd>0, nc1$gdd, 0)
nc1$frz<- ifelse((nc1$Tmin<=-2.2), 1, 0)
nc1$count <- ave(
  nc1$frz, nc1$biweekly, nc1$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
nc1<- nc1 %>%
  filter(doy >= 50) %>%
  filter(doy <= 100)
nc1$Tmin<-ifelse(nc1$Tmin==-9999, NA, nc1$Tmin)
nc1<-na.omit(nc1)

nc.count<- nc1 %>%
  dplyr::select(year, biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
nc.count$mean<-ave(nc.count$count, nc.count$biweekly)
nc.count$stand_dev<-ave(nc.count$count, nc.count$biweekly, FUN=sd)
nc.high<-nc.count%>%group_by(biweekly)%>%summarise(high=max(count))%>%ungroup(nc.count)
nc.low<-nc.count%>%group_by(biweekly)%>%summarise(low=min(count))%>%ungroup(nc.count)
nc.count<-full_join(nc.count, nc.high)
nc.count<-full_join(nc.count, nc.low)
nc2<-nc.count %>%
  ungroup(nc.count) %>%
  dplyr::select(-count, -year) 
nc2<-nc2%>% filter (! duplicated(biweekly))
nc2$site<-"North Carolina, USA"
nc.count$site<-"North Carolina, USA"


# Lyon, France: Apr 5 - May 10
ren<-france %>%
  dplyr::select(STATION_NAME,DATE, TAVG, TMIN, TMAX) %>%
  filter(STATION_NAME == "BRON LYON AEROPORT FR") %>%
  rename(Tmin = TMIN) %>%
  rename(Tmax = TMAX) %>%
  rename(date = DATE)
ren$year <- substr(ren$date, 0, 4)
ren<- ren %>% 
  filter(year>=1965) %>%
  filter(year<2016)
ren$month<- substr(ren$date, 5, 6)
ren$day<- substr(ren$date, 7,8)
ren<- ren %>%
  dplyr::select(-date)%>%
  unite(date, year, month, day, sep="-") %>%
  dplyr::select(date,Tmin, Tmax)
ren$doy<-yday(ren$date)
ren$year<-substr(ren$date,0,4)
ren$month<-substr(ren$date, 6,7)
ren$day<-substr(ren$date,9,10)
ren$biweek<-ifelse(ren$day<=15,1,2)
ren<-ren%>%unite(biweekly,month,biweek, sep="_")
ren$Tmean <- (ren$Tmax + ren$Tmin)/2
ren$gdd <- ren$Tmean - 5
ren$gdd <-ifelse(ren$gdd>0, ren$gdd, 0)
ren$frz<- ifelse((ren$Tmin<=-2.2), 1, 0)
ren$count <- ave(
  ren$frz, ren$biweekly, ren$year,
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
ren<- ren %>%
  filter(doy >= 95) %>%
  filter(doy <= 130)

ren.count<- ren %>%
  dplyr::select(year,biweekly, count) %>%
  group_by(year, biweekly)%>%
  filter(row_number(biweekly)==n())
ren.count$mean<-ave(ren.count$count, ren.count$biweekly)
ren.count$stand_dev<-ave(ren.count$count, ren.count$biweekly, FUN=sd)
ren.high<-ren.count%>%group_by(biweekly)%>%summarise(high=max(count))%>%ungroup(ren.count)
ren.low<-ren.count%>%group_by(biweekly)%>%summarise(low=min(count))%>%ungroup(ren.count)
ren.count<-full_join(ren.count, ren.high)
ren.count<-full_join(ren.count, ren.low)
ren1<-ren.count %>%
  ungroup(ren.count) %>%
  dplyr::select(-count, -year) 
ren1<-ren1%>% filter (! duplicated(biweekly))
ren1$site<-"Rhone-Alps, FR"
ren.count$site<-"Rhone-Alps, FR"


d<-full_join(gm, wash)
d<-full_join(d, nc2)
d<-full_join(d, ren1)
d<-full_join(d, maine)
df<-full_join(gm.count,wash.count)
df<-full_join(df, nc.count)
df<-full_join(df, ren.count)
df<-full_join(df, water.count)



limits <- aes(ymax = mean + stand_dev, ymin=mean - stand_dev)

d$biweekly<-ifelse(d$biweekly=="02_2", "Feb 15-Feb 29", d$biweekly)
d$biweekly<-ifelse(d$biweekly=="03_1", "Mar 1-Mar 14", d$biweekly)
d$biweekly<-ifelse(d$biweekly=="03_2", "Mar 15-Mar 31", d$biweekly)
d$biweekly<-ifelse(d$biweekly=="04_1", "Apr 1-Apr 14", d$biweekly)
d$biweekly<-ifelse(d$biweekly=="04_2", "Apr 15-Apr 30", d$biweekly)
d$biweekly<-ifelse(d$biweekly=="05_1", "May 1-May 14", d$biweekly)
d$biweekly<-ifelse(d$biweekly=="05_2", "May 15-May 31", d$biweekly)

d$high<-as.numeric(d$high)
d$low<-as.numeric(d$low)

d$biweekly <- factor(d$biweekly, levels=c("Feb 15-Feb 29", "Mar 1-Mar 14", "Mar 15-Mar 31", "Apr 1-Apr 14",
                                            "Apr 15-Apr 30","May 1-May 14","May 15-May 31"))
limitcolor<-c("lightgoldenrod", "lightgoldenrod", "lightgoldenrod","plum", "plum", "plum", 
              "lightblue", "lightblue", "lightblue", "lightblue",
              "pink", "pink", "pink", "mediumaquamarine", "mediumaquamarine", "mediumaquamarine", "mediumaquamarine")
ggplot((d), aes(x=biweekly, y=mean, col=site)) + geom_point() + xlab("Two Week Period") + ylab("Average Number of days per two week period below -2.2C") + 
  geom_line(aes(x=biweekly, y=mean, col=site, group=site)) + geom_linerange(limits, col=limitcolor)

risk<-ggplot(d, aes(x=biweekly, y=mean, color=factor(site, labels = c("Bavaria, DE: March 31 - April 30", "Maine, USA: April 10 - May 30", 
                                                                "North Carolina, USA: February 21 - April 4",  "Rhone-Alps, FR: April 5 - May 10", "Washington, USA: March 22 - April 30")),
                    shape=factor(site))) +
         geom_point(aes(color=factor(site), group=site, shape=factor(site)), size=2) + ylab("Number of days below -2.2C per two-week period") + 
  geom_line(aes(x=biweekly, y=mean,  color=factor(site), group=site), size=1) + 
  geom_linerange(aes(ymax=high, color=as.factor(site),  ymin=low), position=position_dodge(.2), alpha=0.3, size=2) + labs(color="Location and Day of Budburst Range") + 
  theme(panel.grid.minor = element_blank(), legend.position="none", axis.title.x=element_blank(), panel.grid.major = element_blank(), 
                                           panel.background = element_blank(), 
                                           panel.border = element_rect(fill=NA), legend.key=element_blank(),
        axis.ticks.x=element_line(), plot.margin=unit(c(-.6,.5,.3,.5),"cm")) +
  scale_x_discrete(breaks=c(53, 66, 82, 98, 114, 128, 144), label=c("Feb 15 - Feb 29", "Mar 1 - Mar 14", "Mar 15 - Mar 31", "Apr 1 - Apr 14",
                                                                                                               "Apr 15 - Apr 30", "May 1 - May 14", "May 15 - May 31"),
                   position="top") +scale_y_continuous(expand = c(0, 0.1)) + coord_cartesian(ylim=c(0.03:15)) +
  annotate("text", x = 6, y = 12, label = "Climate Data", fontface = "bold") + scale_shape_manual(labels=c("Bavaria, DE: March 31 - April 30", "Maine, USA: April 10 - May 30", 
                                                                                                           "North Carolina, USA: February 21 - April 4",  "Rhone-Alps, FR: April 5 - May 10", "Washington, USA: March 22 - April 30"),
                                                                                                  values= c(0, 19, 0, 2, 15))
risk

#theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1), legend.position=c(0.88,0.91),
#      +        legend.key.size=unit(0.3, "cm"), plot.margin=unit(c(.2,.5,-.3,.5),"cm")
      
## Make a dataframe:

x<- data.frame("Site"=c( "Rhone-Alps, France", "Bavaria, Germany", "Maine, USA", "North Carolina, USA","Washington, USA"), "Earliest"=c( 90,90, 100, 50, 80), 
               "Early" = c( 100, 112, 115, 80,100), "Late" = c( 115, 127, 120, 90, 120), "Latest" = c( 120,140, 150, 100, 130))

fix<-x%>%
  gather("time", "doy", Earliest, Early, Late, Latest) %>%
  arrange(desc(Site))

fix$biweekly<-ifelse((fix$doy>=46 & fix$doy <=60), "Feb 15-Feb 29", fix$doy)
fix$biweekly<-ifelse((fix$doy>=61 & fix$doy <=73), "Mar 1-Mar 14", fix$biweekly)
fix$biweekly<-ifelse((fix$doy>=74 & fix$doy <=91), "Mar 15-Mar 31", fix$biweekly)
fix$biweekly<-ifelse((fix$doy>=92 & fix$doy <=106), "Apr 1-Apr 14", fix$biweekly)
fix$biweekly<-ifelse((fix$doy>=107 & fix$doy <=121), "Apr 15-Apr 30", fix$biweekly)
fix$biweekly<-ifelse((fix$doy>=122 & fix$doy <=136), "May 1-May 14", fix$biweekly)
fix$biweekly<-ifelse((fix$doy>=137 & fix$doy <=152), "May 15-May 31", fix$biweekly)

fix$doy<-as.numeric(as.character(fix$doy))

fix2 <- transform(fix, Site = reorder(Site, order(Site, decreasing = TRUE)))
fix$Site <- factor(fix$Site,levels=sort(unique(fix$Site), decreasing=FALSE))
time<- ggplot((fix), aes(y=doy, x=Site, color=Site)) + geom_boxplot(aes(y=doy, x=Site, fill=Site), width=0.2) + coord_flip() +
  scale_y_continuous(breaks=c(55, 70, 85, 101, 116, 131, 146), label=c("Feb15-Feb29", "Mar1-Mar14", "Mar15-Mar31", "Apr1-Apr14",
                                                                       "Apr15-Apr30", "May1-May14", "May15-May31")) +
  theme(legend.position="none", 
        panel.grid.major = element_blank(),panel.grid.minor = element_blank(), panel.background= element_blank(),plot.margin=unit(c(.2,.5,-.3,.5),"cm"),
        panel.border = element_rect(fill=NA)) +
  ylab("") + xlab("") + scale_x_discrete(limits = rev(levels(fix$Site))) +
  annotate("text", x=5.35, y=118, label="March 31-April 30", fontface="bold", color="pink2") +
  annotate("text", x=4.35, y=120, label="April 10-May 30", fontface="bold", color="goldenrod") +
  annotate("text", x=3.35, y=83, label="February 21-April 4", fontface="bold", color="mediumaquamarine") +
  annotate("text", x=2.35, y=107, label="April 5-May 10", fontface="bold", color="lightblue4") +
  annotate("text", x=1.35, y=109, label="March 22-April 30", fontface="bold", color="plum2") +
  annotate("text", x=5.4, y=60, label="Phenology Data", fontface="bold")
time

annotate("text", x = -13, y = 10, label = "Phenology Data", fontface = "bold")
limitcolor<-c("lightgoldenrod", "lightgoldenrod", "lightgoldenrod","plum", "plum", "plum", 
              "lightblue", "lightblue", "lightblue", "lightblue",
              "pink", "pink", "pink", "mediumaquamarine", "mediumaquamarine", "mediumaquamarine", "mediumaquamarine")


  labels = c("Bavaria, Germany: March 31-April 30", "Maine, USA: April 10 - May 30", 
             "North Carolina, USA: February 21 - April 4",  "Rhone-Alps, FR: April 5 - May 10", "Washington, USA: March 22 - April 30"))

#c(0.08,0.22), legend.key.size=unit(0.3, "cm")
#theme(legend.position=c(0.05,0.3), legend.key.size=unit(0.4, "cm"), aspect.ratio=0.3,
#      +        panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1), plot.margin=unit(c(-.2,.5,.2,.5),"cm"))

quartz()
ggarrange(time, risk, nrow=2, heights=c(0.45, 1))

#grid.arrange(risk, time, ncol=1, nrow=2, respect=TRUE)

#grid.draw(rbind(ggplotGrob(risk), ggplotGrob(time)))

ggplot(data = df, aes(x=biweekly, y=count)) + geom_boxplot(aes(fill=site))


df$biweekly<-ifelse(df$biweekly=="02_2", "Feb 15 - Feb 29", df$biweekly)
df$biweekly<-ifelse(df$biweekly=="03_1", "Mar 1 - Mar 14", df$biweekly)
df$biweekly<-ifelse(df$biweekly=="03_2", "Mar 15 - Mar 31", df$biweekly)
df$biweekly<-ifelse(df$biweekly=="04_1", "Apr 1 - Apr 14", df$biweekly)
df$biweekly<-ifelse(df$biweekly=="04_2", "Apr 15 - Apr 30", df$biweekly)
df$biweekly<-ifelse(df$biweekly=="05_1", "May 1 - May 14", df$biweekly)
df$biweekly<-ifelse(df$biweekly=="05_2", "May 15 - May 31", df$biweekly)


#df_table <- table(df$biweekly)
#df_levels <- names(df_table)[order(df_table)]
df$biweekly <- factor(df$biweekly, levels=c("Feb 15 - Feb 29", "Mar 1 - Mar 14", "Mar 15 - Mar 31", "Apr 1 - Apr 14",
                                            "Apr 15 - Apr 30","May 1 - May 14","May 15 - May 31"))
qplot(site, count, data = df, 
      geom = "boxplot", color=biweekly) + 
  xlab("Site")+ylab("Mean number of freeze days")


