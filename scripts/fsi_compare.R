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

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# Integrate Phenology Data and compare methodologies
# Set Working Directory
setwd("~/Documents/git/springfreeze/input")

# Compares FSI values for small dataset of Dr O'Keefe's observational
# data and USNPN Spring Indices, based on -1.7 deg Celcius last freeze
method<-read.csv("method.test.csv",header=TRUE,sep=",")

bb.table<-method %>%
  dplyr::select(year,frz_2.2,bb_npn, sm.bb, bb_cam) %>%
  filter(year>=2008)%>%
  filter(year<2015)%>%
  rename("Last Freeze"=frz_2.2)%>%
  rename("Observed"=sm.bb)%>%
  rename("SI-x"=bb_npn)
bb.table$okeefe<-bb.table$`Last Freeze`-bb.table$Observed
bb.table$PhenoCam<-bb.table$`Last Freeze`-bb.table$bb_cam
bb.table$npn<-bb.table$`Last Freeze`-bb.table$`SI-x`
FSI.table<- bb.table %>% 
  dplyr::select(year, npn, okeefe, PhenoCam)%>%
  filter(year>=2008) %>%
  filter(year<2015)
blend<-FSI.table %>% 
  gather(Method, FSI, -year) %>%
  arrange(year) 

cols<-colorRampPalette(brewer.pal(9,"Dark2"))(3)
methodplot<-ggplot(blend, aes(year, FSI)) + xlab("Year") +
  ylab("False Spring Index") + scale_x_continuous(limits = c(2008,2014),breaks=seq(2008,2014,1)) +
  geom_point(aes(shape=Method)) + 
  scale_linetype_manual(name="Method", values=c(4,3,1), labels=c("SI-x", "Observational (O`Keefe)", "PhenoCam")) +
  geom_line(aes(x=year, y=FSI, linetype=Method)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                         panel.background = element_blank(), 
                                                         axis.line = element_line(colour = "black"), legend.key=element_blank(), plot.title = element_text(family="Helvetica"))  + geom_hline(yintercept=0, linetype="solid", col="grey") + geom_hline(yintercept=7, linetype="dotted", col="grey") + 
  scale_shape_manual(name="Method", labels = c("SI-x", "Observational (O`Keefe)", "PhenoCam"),
                     values = c(16, 17, 15)) #+ ggtitle("Calculating False Spring Risk")
quartz()
plot(methodplot)
