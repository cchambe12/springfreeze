##### 10 October 2017 - Cat
# Duration of Vegetative Risk Dan's data for model

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Analysis of bud burst experiment Flynn and Wolkovich, 2018: used now for change in DVR 

library(ggplot2)
library(dplyr)
library(brms)
library(RColorBrewer)

setwd("~/Documents/git/springfreeze/")

if(FALSE){
source('scripts/stan/savestan.R')
dx<-read.csv("output/expdata_dvr.csv", header=TRUE)

dx$sp <- as.numeric(as.factor(dx$sp))
dx<-dx[!is.na(dx$risk),]
levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$warm<-ifelse(dx$warm==15, 0, 1)
dx$photo <- as.numeric(dx$photo)
dx$photo<-ifelse(dx$photo==8, 0, 1)
dx$chill <- as.numeric(dx$chill)
dx$chill1 = ifelse(dx$chill == 1, 1, 0) 
dx$chill2 = ifelse(dx$chill == 2, 1, 0) 
dxb <- dx[!is.na(dx$risk),]


dxb<-filter(dxb, risk>0)

dxb$force<-dxb$warm

fit.brm2<-brm(risk~ force + photo + chill1 + chill2 + force:photo + force:chill1 + force:chill2 +
               photo:chill1 + photo:chill2 + (force + photo + chill1 + chill2 + force:photo + 
                                                force:chill1 + force:chill2 +
                                                photo:chill1 + photo:chill2|sp), data=dxb)

save(fit.brm2, file="~/Documents/git/springfreeze/output/exp_output.Rdata")
}

load(file="output/exp_output.Rdata")

m<-fit.brm2
m.int<-posterior_interval(m)
sum.m<-summary(m)
cri.f<-as.data.frame(sum.m$fixed[,c("Estimate", "l-95% CI", "u-95% CI")])
cri.f<-cri.f[-1,] #removing the intercept 
fdf1<-as.data.frame(rbind(as.vector(cri.f[,1]), as.vector(cri.f[,2]), as.vector(cri.f[,3])))
fdf2<-cbind(fdf1, c(0, 0, 0) , c("Estimate", "2.5%", "95%"))
names(fdf2)<-c(rownames(cri.f), "sp", "perc")

cri.r<-(ranef(m, summary = TRUE, robust = FALSE,
              probs = c(0.025, 0.975)))$sp
cri.r2<-cri.r[, ,-1]
cri.r2<-cri.r2[,-2,]
dims<-dim(cri.r2)
twoDimMat <- matrix(cri.r2, prod(dims[1:2]), dims[3])
mat2<-cbind(twoDimMat, c(rep(1:9, length.out=27)), rep(c("Estimate", "2.5%", "95%"), each=9))
df<-as.data.frame(mat2)
names(df)<-c(rownames(cri.f), "sp", "perc")
dftot<-rbind(fdf2, df)
dflong<- tidyr::gather(dftot, var, value, force:`photo:chill2`, factor_key=TRUE)

#adding the coef estiamtes to the random effect values 
for (i in seq(from=1,to=nrow(dflong), by=30)) {
  for (j in seq(from=3, to=29, by=1)) {
    dflong$value[i+j]<- as.numeric(dflong$value[i+j]) + as.numeric(dflong$value[i])
  }
}
dflong$rndm<-ifelse(dftot$sp>0, 2, 1)
dfwide<-tidyr::spread(dflong, perc, value)
dfwide[,4:6] <- as.data.frame(lapply(c(dfwide[,4:6]), as.numeric ))
dfwide$sp<-as.factor(dfwide$sp)
## plotting

pd <- position_dodgev(height = -0.5)

dfwide$legend<-factor(dfwide$sp,
                      labels=c("Overall Effects","1","2","3","4","5","6","7","8","9"))


### Now make a new dataframe for more conceptual figure
configprep<-subset(dfwide, select=c("sp", "var", "Estimate"))
species<-c("1", "6", "8")
configprep<-subset(configprep, sp%in%species)
configprep<-configprep[!(configprep$var=="photo:chill1" | configprep$var=="photo:chill2" | configprep$var=="force:chill1" 
                 | configprep$var=="chill1"),]
configprep$var<-ifelse(configprep$var=="chill2", "chill", as.character(configprep$var))
configprep$var<-ifelse(configprep$var=="force:chill2", "force:chill", as.character(configprep$var))


estimates<-c("More Forcing", "Shorter Photoperiod", "Less Chilling", "Interaction of More Forcing \nand Shorter Photoperiod", "Interaction of More Forcing \nand Less Chilling",
             "More Forcing and \nShorter Photoperiod", "More Forcing and \nLess Chilling")


configprep$Jvar<-NA
configprep$Jvar<-ifelse(configprep$var=="force", 7, configprep$var)
configprep$Jvar<-ifelse(configprep$var=="photo", 6, configprep$Jvar)
configprep$Jvar<-ifelse(configprep$var=="chill", 5, configprep$Jvar)
configprep$Jvar<-ifelse(configprep$var=="force:photo", 2, configprep$Jvar)
configprep$Jvar<-ifelse(configprep$var=="force:chill", 1, configprep$Jvar)
configprep$Jvar2<-as.numeric(configprep$Jvar)

configprep$spp<-NA
configprep$spp<-ifelse(configprep$sp=="1", 1, configprep$spp)
configprep$spp<-ifelse(configprep$sp=="6", 2, configprep$spp)
configprep$spp<-ifelse(configprep$sp=="8", 3, configprep$spp)
configprep$spp<-ifelse(configprep$spp>1, (configprep$spp*0.1) - 0.1, 0)
configprep$Jvar2<-as.numeric(configprep$Jvar)-configprep$spp

configprep$newEstimate<-configprep$Estimate
configprep$newEstimate<-ifelse(configprep$var=="photo", -configprep$Estimate, configprep$newEstimate)
configprep$newEstimate<-ifelse(configprep$var=="chill", -configprep$Estimate, configprep$newEstimate)
configprep$newEstimate<-ifelse(configprep$var=="force:photo", -configprep$Estimate, configprep$newEstimate)
configprep$newEstimate<-ifelse(configprep$var=="force:chill", -configprep$Estimate, configprep$newEstimate)

configprep$sp<-as.numeric(configprep$sp)-1
species<-unique(configprep$sp)
configprep$est2<-configprep$newEstimate
for(i in c(1:length(species))) {
  configprep$est2<-ifelse(configprep$sp==species[i] & configprep$var=="force:photo", configprep$newEstimate[configprep$var=="force" & configprep$sp==species[i]]+
                        configprep$newEstimate[configprep$var=="photo" & configprep$sp==species[i]]+
                        configprep$newEstimate[configprep$var=="force:photo" & configprep$sp==species[i]], configprep$est2)
  configprep$est2<-ifelse(configprep$sp==species[i] & configprep$var=="force:chill", configprep$newEstimate[configprep$var=="force" & configprep$sp==species[i]]+
                        configprep$newEstimate[configprep$var=="chill" & configprep$sp==species[i]]+
                        configprep$newEstimate[configprep$var=="force:chill" & configprep$sp==species[i]], configprep$est2)

}


configprep<-rbind(configprep,list(1,"force:photo2",0, 3, 4, 0, 0, configprep$newEstimate[configprep$sp==1 & configprep$var=="force:photo"], 0))
configprep<-rbind(configprep,list(6,"force:photo2",0, 3, 3.9, 0, 0, configprep$newEstimate[configprep$sp==6 & configprep$var=="force:photo"], 0))
configprep<-rbind(configprep,list(8,"force:photo2",0, 3, 3.8, 0, 0, configprep$newEstimate[configprep$sp==8 & configprep$var=="force:photo"], 0))
configprep<-rbind(configprep,list(1,"force:chill2",0, 1, 3, 0, 0, configprep$newEstimate[configprep$sp==1 & configprep$var=="force:chill"], 0))
configprep<-rbind(configprep,list(6,"force:chill2",0, 1, 2.9, 0, 0, configprep$newEstimate[configprep$sp==6 & configprep$var=="force:chill"], 0))
configprep<-rbind(configprep,list(8,"force:chill2",0, 1, 2.8, 0, 0, configprep$newEstimate[configprep$sp==8 & configprep$var=="force:chill"], 0))


estimates<-rev(estimates)

cols <- colorRampPalette(brewer.pal(3,"Accent"))(3)
expB<-ggplot(configprep, aes(x=0, xend=est2, y=Jvar2, yend=Jvar2)) +
  geom_vline(xintercept=0, linetype="dotted") +
  scale_linetype_manual(name="Species", values=c("solid", "longdash", "dotdash"),
                      labels=c("1"=expression(paste(italic("Acer pensylvanicum"))),
                               "6"=expression(paste(italic("Fagus grandifolia"))),
                               "8"=expression(paste(italic("Populus grandidentata"))))) +
  scale_color_manual(name="Species", values=cols,
                        labels=c("1"=expression(paste(italic("Acer pensylvanicum"))),
                                 "6"=expression(paste(italic("Fagus grandifolia"))),
                                 "8"=expression(paste(italic("Populus grandidentata"))))) +
  geom_segment(arrow = arrow(length = unit(0.02, "npc")), aes(linetype=as.factor(sp), col=as.factor(sp))) +
  scale_y_discrete(limits = sort(unique(configprep$var)), labels=estimates) +
  xlab("Change in Duration (Days) \nof Vegetative Risk") + ylab("") +
  geom_hline(yintercept=2.5, col="grey") + 
  annotate("text", x = -14.3, y = 2.35, label = "Combined Effects:", size=4, family="Helvetica") +
  annotate("text", x = -11.9, y = 7.45, label = "Estimated Isolated Effects:", size=4, family="Helvetica") + 
  theme_linedraw() +
  theme(legend.text=element_text(size=9), legend.title = element_text(size=9),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="Helvetica"),
        legend.text.align = 0) + coord_cartesian(ylim=c(1,7), xlim=c(-20, 10))
quartz()
expB

png("figures/exp_intrxns_dvr_color.png", 
    width=8, units="in",
    height=6,res = 750 )
plot(expB)
dev.off()
