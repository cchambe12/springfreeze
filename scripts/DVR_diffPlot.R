## Most updated script for Dan's experiment plots
# Cat - 1 November 2017

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
d<-read.csv("input/Budburst.clean.csv",header=TRUE)

tx<-c("CS0", "WL1")
dx<- d %>%
  dplyr::select(ind, treatcode, lday, bday, site) %>%
  filter(treatcode %in% tx)
  
## Omit Viburnum due to budburst date issues
dx<-na.omit(dx)
dx$species<-substr(dx$ind, 1, 6)
dx<-dx%>%filter(species!="VIBCAS")%>%filter(species!="VIBLAN") # all entries for two species have the same budburst and leafout day, removed because probably from error
small.spp<-dx %>% dplyr::select(species, treatcode) %>% filter(treatcode=="WL1")
spp<-unique(small.spp$species)
dx<-dx%>% filter(species %in% spp)
dx<-dplyr::select(dx, -ind, -site)


### Start determining risk for each treatment
dxx<-dx
dxx$risk<-dxx$lday-dxx$bday
dxx$dvr<-ave(dxx$risk, dxx$species, dxx$treatcode)
dxx$dvr.sd<-ave(dxx$risk, dxx$species, dxx$treatcode, FUN=sd)/sqrt(length(unique(dxx$dvr)))
dxx<-dplyr::select(dxx, species, treatcode, dvr, dvr.sd)
dxx<-dxx[!duplicated(dxx),]
dxx$tx.sd<-paste(dxx$treatcode, "se", sep="_")
dm<-dxx%>%dplyr::select(species, treatcode, dvr)%>%spread(treatcode, dvr)
ds<-dxx%>%dplyr::select(species, tx.sd, dvr.sd)%>%spread(tx.sd, dvr.sd)
dxx<-inner_join(dm, ds)
dxx$diff<-dxx$CS0-dxx$WL1
dxx$diff.sd<-sqrt((dxx$CS0_se)^2+(dxx$WL1_se)^2) 
df<-dx%>%filter(treatcode=="WL1")%>%dplyr::select(treatcode, bday, species)
df$bday<-ave(df$bday, df$species, df$treatcode)
df<-df[!duplicated(df),]


df$species<-ifelse(df$species=="ILEMUC", "I. mucronata", df$species)
df$species<-ifelse(df$species=="ACEPEN", "A. pensylvanicum", df$species)
df$species<-ifelse(df$species=="BETALL", "B.alleghaniensis", df$species)
df$species<-ifelse(df$species=="BETPAP", "B.papyrifera", df$species)
df$species<-ifelse(df$species=="ACERUB", "A.rubrum", df$species)
df$species<-ifelse(df$species=="POPGRA", "P.grandidentata", df$species)
df$species<-ifelse(df$species=="QUERUB", "Q.rubra", df$species)
df$species<-ifelse(df$species=="ACESAC", "A.saccharum", df$species)
df$species<-ifelse(df$species=="FAGGRA", "F.grandifolia", df$species)

df$code<-reorder(df$species, df$bday)
dxx$code<-df$code

#write.csv(dxx, file="~/Documents/git/springfreeze/output/diffplot.csv", row.names = FALSE)

diff<-ggplot(dxx, aes(x=factor(code), y=diff)) + geom_point() + 
    geom_linerange(aes(ymin=diff-diff.sd, ymax=diff+diff.sd), alpha=0.3) + 
  ylab(expression(Delta*" in DVR between treatments")) + coord_cartesian(ylim=c(0,25)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, vjust=0.5), axis.text=element_text(size=10))
plot(diff)
  

ggplot(dxx, aes(x=CS0, y=WL1)) + geom_point(aes(size=diff), shape=21) + 
  geom_linerange(aes(ymin=WL1-WL1_se, ymax=WL1+WL1_se), alpha=0.3) +
  geom_errorbarh(aes(xmax = CS0+CS0_se, xmin = CS0-CS0_se, height = 0), alpha=0.3) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_text(aes(label=code), vjust=2) + xlab("DVR with weak treatment effects") + 
  ylab("DVR with strong treatment effects") + 
  scale_size_continuous(name=expression(Delta*" in DVR"))







