# Cat - 16 Feb 2017
## Write a script that looks at the latitudinal gradient
## And frequency of FSI events as the years progress; 30 year timeframe

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Set Working Directory
setwd("~/Documents/git/springfreeze")
d<-read.csv("input/fsi.freq.csv",header=TRUE)
d<- within(d, false.springs[false.springs=="N/A"]<-NA)
d<-na.omit(d)

freq<-as.data.frame(table(d$false.springs))%>%
  rename(false.springs=Var1)%>%
  rename(fsi=Freq)
freq<-full_join(freq, d, by="false.springs")
eur<-freq%>%
  rename(year=false.springs)%>%
  dplyr::select(-Station)%>%
  filter(region=="europe")
eur$year<-as.numeric(as.character(eur$year))
ggplot((eur), aes(x=year, y=fsi)) + geom_point() + 
  geom_smooth(method="lm")
mod<-lm(fsi~year, data=eur)
summary(mod)
both<-freq%>%
  rename(year=false.springs)%>%
  dplyr::select(-Station)
both$year<-as.numeric(as.character(both$year))
ggplot((both), aes(x=year, y=fsi)) + geom_point(aes(col=region)) + 
  geom_smooth(method="lm")
mod1<-lm(fsi~year, data=both)
summary(mod1)
am<-freq%>%
  rename(year=false.springs)%>%
  dplyr::select(-Station)%>%
  filter(region=="n.america")
am$year<-as.numeric(as.character(am$year))
ggplot((am), aes(x=year, y=fsi)) + geom_point(aes(col=region)) + 
  geom_smooth(method="lm")
mod2<-lm(fsi~year, data=am)
summary(mod2)
