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
metdata<-read.csv("input/metdata-norway.csv",header=TRUE)
attach(metdata)

library(splitstackshape)
met<-cSplit(metdata, "X4780.GARDERMOEN..TAM.1966", ";")
newname<-list(as.character("Date","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
names(met)<-c("col1","col2","col3","col4","col5","col6","col7","col8","col9","col10","col11",
              "col12","col13")
metchange<- met %>%
rename(col2 = jan)

library(reshape)
met.melt <- melt(met, id=("col1"))

write.csv(met, "metsplit.csv")
