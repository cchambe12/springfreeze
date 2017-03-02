## Running Anova's for DanF's data
# In Buds folder using Dan Flynn's data
## LEARN HOW TO LOOP FOR EACH SPECIES AND CALCULATING RISK!!!! ##

## 10 Feb 2017 - Cat
# Using Dan's Data!!

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
library(car)
library(broom)
library(lme4)
#packageurl <- "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-4.tar.gz" 
#install.packages(packageurl, repos=NULL, type="source")

# Set Working Directory
setwd("~/Documents/git/springfreeze")
d<-read.csv("input/Budburst.DF.csv",header=TRUE)

d$DOY<-yday(d$Date)
d$chilling<- as.factor(substr(d$chill, 6, 6))
#d$chilling<-as.numeric(as.character(
  #ifelse((d$chilling==0), 0, ifelse((d$chilling==1), 4, 1.5))))
d$force<-as.numeric(as.character(ifelse((d$warm=="warm"), 20, 15)))
d$photoperiod<- as.numeric(as.character(ifelse((d$photo=="short"), 8, 12)))
phases<-c("4","7")
d<-d %>%
  dplyr::select(id, sp, site, tleaf, DOY, chilling, force, photoperiod, treatcode) %>%
  filter(tleaf %in% phases)
d$tleaf<- factor(d$tleaf, levels = c(4,7), 
                        labels = c("Budburst","Leaves"))

## Harvard Forest Data
spp<-c("ACEPEN", "ACERUB", "BETALL", "BETPAP", "ILEMUC", "POPGRA", "QUERUB")
d.hf<-d%>%
  filter(site=="HF") %>%
  filter(sp %in% spp) %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.hf$risk<-d.hf$Leaves-d.hf$Budburst 
d.hf<-filter(d.hf,risk>0)
d.hf<-na.omit(d.hf)
#hf<-d.hf%>%
  #group_by(sp) %>% 
  #do(tidy(lm(risk~chilling + force + photoperiod + (chilling*force) + 
                      #(chilling*photoperiod) + (force*photoperiod), data=.), type="II"))

species <- unique(d.hf$sp)

models<-lmList(risk~chilling+force+photoperiod|sp,data=d.hf)
models<-na.omit(models)
Anova(models)
models <- sapply(species, function(my) {
  lm(risk~chilling+force+photoperiod,data=d.hf)
})

ANOVA.tables <- sapply(models, aov, simplify=FALSE)


storing<- list()
species<-unique(d.hf$sp)
for(i in 1:length(species)){
  #dat=subset=d.hf$sp[i]
  storing[[i]]<-lm(risk~chilling+force+photoperiod,data=d.hf, subset=d.hf$sp[i])
}

storing.anovas<-list() #list(list()) - try second time
for(i in 1:length(species)){
  for(j in 1:length(species)){
    #if(i!=j){
    storing.anovas[[i]]<-anova(storing[[i]],storing[[j]])
    #}
  }
}

model<-lm(risk~chilling+force+photoperiod,data=d.hf,type="II")
model1<-lm(risk~chilling+force+photoperiod+(chilling*force) + 
             (chilling*photoperiod) + (force*photoperiod),data=d.hf,type="II")
Anova(model)
Anova(model)

# Make .csv file with number of individs per spp per tx HF
df.hf<-as.data.frame(table(d.hf$sp,d.hf$treatcode))%>%
  rename(species=Var1)%>%
  rename(tx=Var2)
write.csv(df.hf, "~/Documents/git/springfreeze/output/danfs.tx.hf.csv", row.names=FALSE)

## Saint-Hipp Data
d.sh<-d%>%
  filter(site=="SH") %>%
  group_by(sp, id, tleaf)%>%
  arrange(id)%>%
  filter(row_number()==1) %>%
  spread(tleaf, DOY)
d.sh$risk<-d.sh$Leaves-d.sh$Budburst 
d.sh<-filter(d.sh,risk>0)

sh<-d.sh%>%
  group_by(sp) %>% 
  do(tidy(aov(risk~chilling + force + photoperiod + (chilling*force) + 
                (chilling*photoperiod) + (force*photoperiod), data=.), type="II"))
mod2<-lmer(risk~chilling + force + photoperiod + (1|sp), data=d.sh)
mod3<-lmer(risk~chilling + force + photoperiod + chilling*force + chilling*photoperiod +
             force*photoperiod + (1|sp), data=d.sh)
write.csv(sh, "~/Documents/git/springfreeze/output/dan.sh.anova.csv", row.names=FALSE)

# Make .csv file with number of individs per spp per tx SH
df.sh<-as.data.frame(table(d.sh$sp,d.sh$treatcode))%>%
  rename(species=Var1)%>%
  rename(tx=Var2)
write.csv(df.sh, "~/Documents/git/springfreeze/output/danfs.tx.sh.csv", row.names=FALSE)
