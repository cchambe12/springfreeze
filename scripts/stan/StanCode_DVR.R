##### 10 October 2017 - Cat
# Duration of Vegetative Risk Dan's data for model

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()


forlatex = TRUE # set to FALSE if just trying new figures, TRUE if outputting for final
runstan = TRUE # set to TRUE to actually run stan models. FALSE if loading from previous runs

# Analysis of bud burst experiment 2015. 

library(ggplot2)
#library(rstan)
library(rstanarm)
#library(shinystan)
#library(bayesplot)
library(dplyr)
library(gridExtra)
library(brms)
library(egg)
library(ggstance)
library(RColorBrewer)

setwd("~/Documents/git/springfreeze/")
source('scripts/stan/savestan.R')
dx<-read.csv("output/danfdata.csv", header=TRUE)
#dx<-read.csv("output/fakedata_dvr.csv", header=TRUE)
#dx<-read.csv("output/danf_short.csv", header=TRUE)
#dfwide<-read.csv("output/df_modforplot.csv", header=TRUE)

# Prep 
#dx<-dx[!(dx$chill==1),]
#dx$chill<-ifelse(dx$chill==2, 0, 1)
#dx<-dx%>%filter(species!="VIBCAS")%>%filter(species!="VIBLAN")
dx$sp <- as.numeric(as.factor(dx$sp))
#dx$site <- as.numeric(as.factor(dx$site))
dx<-dx[!is.na(dx$risk),]
levels(dx$warm) = c(0,1); levels(dx$photo) = c(0, 1); levels(dx$chill) = 1:3
dx$warm <- as.numeric(dx$warm)
dx$warm<-ifelse(dx$warm==15, 0, 1)
dx$photo <- as.numeric(dx$photo)
dx$photo<-ifelse(dx$photo==8, 0, 1)
dx$chill <- as.numeric(dx$chill)
#dx$site<- as.numeric(dx$site)
# Chill dummy variables
dx$chill1 = ifelse(dx$chill == 1, 1, 0) 
dx$chill2 = ifelse(dx$chill == 2, 1, 0) 

#with(dx, table(chill1, chill2)) # all three levels in here

dxb <- dx[!is.na(dx$risk),]


# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Important: Fixing the 1/2 issue to 0/1 here
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
unique(dxb$warm)
#dxb$warm[dxb$warm==1] <- 0
#dxb$warm[dxb$warm==2] <- 1

unique(dxb$photo)
#dxb$photo[dxb$photo==1] <- 0
#dxb$photo[dxb$photo==2] <- 1

#unique(dxb$site)
#dxb$site[dxb$site==1] <- 0
#dxb$site[dxb$site==2] <- 1

dxb<-filter(dxb, risk>0)

#unique(dxb$chill1)
#unique(dxb$chill2)

risk = as.numeric(dxb$risk) # dvr as response 
warm = dxb$warm
#site = as.numeric(dxb$site)
sp = as.numeric(dxb$sp) 
photo = dxb$photo
chill1 = as.numeric(dxb$chill1)
chill2 = as.numeric(dxb$chill2)
N = length(risk) 
n_sp = length(unique(dxb$sp))
#n_site = length(unique(dxb$site))

datalist.b<-list(risk=risk, warm=warm, sp=sp, photo=photo, chill1=chill1, chill2=chill2, N=N, n_sp=n_sp)
# 1. Budburst day. 
if(runstan){
  datalist.b <- list(risk = dxb$risk, # dvr as response 
                     warm = as.numeric(dxb$warm),
                     sp = as.numeric(dxb$sp), 
                     photo = as.numeric(dxb$photo), 
                     chill1 = as.numeric(dxb$chill1),
                     chill2 = as.numeric(dxb$chill2),
                     N = length(risk), 
                     n_sp = length(unique(dxb$sp))
  )
  
    doym.b <- stan('scripts/stan/dvr_sp_chill_inter_pool.stan', ### change when divergent transitions improve!!
                 data = datalist.b, warmup=1500, iter = 2000, chains = 2,
                 control = list(adapt_delta = 0.99))
                 #               , max_treedepth = 15)) 
  
}

#dxb$force<-dxb$warm
fit1<-stan_glmer(risk~ force + photo + chill1 + chill2 + force:photo + force:chill1 + force:chill2 +
                 photo:chill1 + photo:chill2 + (1|sp), data=dxb)
fit1

dxb$force<-dxb$warm
#fit.brm<-brm(risk~ force + photo + chill + force:photo + force:chill +
 #             photo:chill + (force + photo + chill + force:photo + force:chill +
  #                                      photo:chill|sp), data=dxb)
fit.brm2<-brm(risk~ force + photo + chill1 + chill2 + force:photo + force:chill1 + force:chill2 +
               photo:chill1 + photo:chill2 + (force + photo + chill1 + chill2 + force:photo + 
                                                force:chill1 + force:chill2 +
                                                photo:chill1 + photo:chill2|sp), data=dxb)
#fit.check<-brm(risk~force + photo + chill1  + chill2 + force:photo + force:chill1 + force:chill2 +
 #                photo:chill1 + photo:chill2 + (1|sp) + (force-1|sp) + (photo-1|sp) + (chill1-1|sp) +
  #               (chill2-1|sp) + (force:photo-1|sp) + (force:chill1-1|sp) + (force:chill2-1|sp) +
   #              (photo:chill1-1|sp) + (photo:chill2-1|sp), data=dxb)

#fit.brm.check<-brm(risk~ force + photo + chill + force:photo + force:chill +
 #              photo:chill + (force + photo + chill + force:photo + force:chill +
  #                              photo:chill-1|sp), data=dxb)

#fit<-stan_glmer(risk~ force + photo + chill + force:photo + force:chill +
 #              photo:chill + (force + photo + chill + force:photo + force:chill +
  #                              photo:chill|sp), data=dxb)


save(fit.brm2, file="~/Documents/git/springfreeze/output/exp_output.Rdata")
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

estimates<-c("Forcing", "Photoperiod", "Chilling 1.5", "Chilling 4", "Forcing x Photoperiod", 
             "Forcing x Chilling 1.5", "Forcing x Chilling 4", "Photoperiod x Chilling 1.5", "Photoperiod x Chilling 4")
dfwide$legend<-factor(dfwide$sp,
                      labels=c("Overall Effects","1","2","3","4","5","6","7","8","9"))
estimates<-rev(estimates)
#write.csv(dfwide, file="~/Documents/git/springfreeze/output/df_modforplot.csv", row.names=FALSE)
fig1 <-ggplot(dfwide, aes(x=Estimate, y=var, color=legend, size=factor(rndm), alpha=factor(rndm)))+
  geom_point(position =pd)+
  geom_errorbarh(aes(xmin=(`2.5%`), xmax=(`95%`)), position=pd, size=.5, height =0, width=0)+
  geom_vline(xintercept=0)+
  scale_colour_manual(values=c("blue", "firebrick3", "orangered1","orange3", "sienna4","sienna2", "green4", "green3", "purple2", "magenta3"),
                      breaks=c("Overall Effects"))+
  scale_size_manual(values=c(3, 2, 2, 2, 2, 2, 2, 2, 2, 2)) +
  scale_shape_manual(labels="", values=c("1"=16,"2"=16))+
  scale_alpha_manual(values=c(1, 0.5)) +
  guides(size=FALSE, alpha=FALSE) + #removes the legend 
  ggtitle(label = "A.")+ 
  scale_y_discrete(limits = rev(unique(sort(dfwide$var))), labels=estimates) + ylab("") + 
  labs(col="Effects") + theme(legend.position = "none", legend.box.background = element_rect(), 
                              legend.title=element_blank(), legend.key.size = unit(0.05, "cm"), 
                              panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank()) +
  xlab(expression(atop("Model Estimate of Change ", paste("in Duration of Vegetative Risk (days)"))))
fig1

### Now make a new dataframe for more conceptual figure
## Do I add the intercept? Which is 23.71... 
simple<-subset(dfwide, select=c("sp", "var", "Estimate"))
#simple<-simple[(simple$sp=="1"|simple$sp=="4"|simple$sp=="8"),]
#simple$sp<-ifelse(simple$sp=="4", "1", "9")
simple<-simple[!(simple$sp=="0"),]
simple<-simple[!(simple$var=="photo:chill1" | simple$var=="photo:chill2" | simple$var=="force:chill1" 
                 | simple$var=="chill1"),]
simple$var<-ifelse(simple$var=="chill2", "chill", as.character(simple$var))
simple$var<-ifelse(simple$var=="force:chill2", "force:chill", as.character(simple$var))


estimates<-c("More Forcing", "Shorter Photoperiod", "Less Chilling", "More Forcing and \nShorter Photoperiod", 
             "More Forcing and \nLess Chilling")

#pd <- position_dodgev(height = -0.5)
#simple$Jvar <- ave(as.numeric(simple$var), simple$var, 
 #                         FUN = function(x) x + rnorm(length(x), sd = .1))
simple$Jvar<-NA
simple$Jvar<-ifelse(simple$var=="force", 5, simple$var)
simple$Jvar<-ifelse(simple$var=="photo", 4, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="chill", 3, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="force:photo", 2, simple$Jvar)
simple$Jvar<-ifelse(simple$var=="force:chill", 1, simple$Jvar)
simple$Jvar2<-as.numeric(simple$Jvar)

simple$spp<-(as.numeric(simple$sp)-1)
simple$spp<-ifelse(simple$spp>1, (simple$spp*0.05) - 0.05, 0)
simple$Jvar2<-as.numeric(simple$Jvar)-simple$spp

#simple$Jvar2<-ifelse(simple$sp=="4", simple$Jvar2-0.1, simple$Jvar2)
#simple$Jvar2<-ifelse(simple$sp=="8", simple$Jvar2-0.2, simple$Jvar2)

simple$Estimate<-ifelse(simple$var=="photo", -simple$Estimate, simple$Estimate)
simple$Estimate<-ifelse(simple$var=="chill", -simple$Estimate, simple$Estimate)
simple$Estimate<-ifelse(simple$var=="force:photo", -simple$Estimate, simple$Estimate)
simple$Estimate<-ifelse(simple$var=="force:chill", -simple$Estimate, simple$Estimate)

species<-unique(simple$sp)
simple$est2<-simple$Estimate
for(i in c(1:length(species))) {
  simple$est2<-ifelse(simple$sp==species[i] & simple$var=="force:photo", simple$Estimate[simple$var=="force" & simple$sp==species[i]]+
                        simple$Estimate[simple$var=="photo" & simple$sp==species[i]]+
                        simple$Estimate[simple$var=="force:photo" & simple$sp==species[i]], simple$Estimate)
  simple$est2<-ifelse(simple$sp==species[i] & simple$var=="force:chill", simple$Estimate[simple$var=="force" & simple$sp==species[i]]+
                        simple$Estimate[simple$var=="chill" & simple$sp==species[i]]+
                        simple$Estimate[simple$var=="force:chill" & simple$sp==species[i]], simple$est2)

}


#simple$est3<-simple$est2+23.71


estimates<-rev(estimates)
cols <- colorRampPalette(brewer.pal(9,"Spectral"))(9)
exp<-ggplot(simple, aes(x=0, xend=est2, y=Jvar2, yend=Jvar2, col=sp)) +
  geom_vline(xintercept=0, linetype="dotted") +
  scale_colour_manual(name="Species", values=cols,
                      labels=c("1"=expression(paste(italic("Acer pensylvanicum"))),
                               "2"=expression(paste(italic("Acer rubrum"))),
                               "3"=expression(paste(italic("Acer saccharum"))),
                               "4"=expression(paste(italic("Betula alleghaniensis"))),
                               "5"=expression(paste(italic("Betula papyrifera"))),
                               "6"=expression(paste(italic("Fagus grandifolia"))),
                               "7"=expression(paste(italic("Ilex mucronata"))),
                               "8"=expression(paste(italic("Populus grandidentata"))),
                               "9"=expression(paste(italic("Quercus rubra"))))) + 
  geom_segment(arrow = arrow(length = unit(0.03, "npc"))) +
  scale_y_discrete(limits = sort(unique(simple$var)), labels=estimates) + 
  xlab("Change in Duration (Days) \nof Vegetative Risk") + ylab("") +
  geom_hline(yintercept=2.5, col="grey") + 
  annotate("text", x = -12.35, y = 2.4, label = "Combined Effects:", fontface="bold", size=3) +
  annotate("text", x = -12.8, y = 5.5, label = "Simple Effects:", fontface="bold", size=3) + 
  theme_linedraw() +
  theme(legend.text=element_text(size=8), legend.title = element_text(size=9), legend.background = element_rect(linetype="solid", color="grey", size=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        text=element_text(family="sans")) + coord_cartesian(ylim=c(1,5))
quartz()
exp

######################################################################################
##################### Making two plots for manuscript ################################
dxx<-read.csv("output/diffplot.csv", header=TRUE)

diff<-ggplot(dxx, aes(x=factor(code), y=diff, col=factor(code))) + geom_point(alpha=0.5) + 
  geom_linerange(aes(ymin=diff-diff.sd, ymax=diff+diff.sd), alpha=0.5) + 
  ylab(expression(atop("Change in Duration of Vegetative Risk (days)")))  +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.title.x=element_blank(),
        axis.text.x = element_text(face = "italic", angle=45, hjust=1),
        axis.text=element_text(size=10), legend.position = "none") +
  scale_colour_manual(values=c("firebrick3", "orangered1","orange3", "sienna4","sienna2", "green4", "green3", "purple2", "magenta3")) +
  ggtitle(label="B.")
plot(diff)

grid.draw(fig1, diff, ncol=2)
quartz()
ggarrange(fig1, diff, ncol=2)


######################################################################################
######################################################################################


# yb = dxb$bday # for shinystan posterior checks
launch_shinystan(doym.b) 

sumer <- summary(fit1)
sumer[grep("", rownames(sumer)),]

betas <- as.matrix(doym.b, pars = c("mu_b_warm","mu_b_photo","mu_b_chill1", "mu_b_chill2",
                                     "b_warm", "b_photo", "b_chill1", "b_chill2"))
betas <- as.matrix(fit1, pars = c("force", "photo", "chill1", "chill2", "force:photo",
                                  "force:chill1", "force:chill2", "photo:chill1", "photo:chill2"))
mcmc_intervals(betas) + annotate("text", x = -13, y = 10, label = "B.", fontface = "bold")
mcmc_intervals(betas)

save(doym.b, file="~/Documents/git/springfreeze/scripts/stan/risk_site_sp_fakedata.Rda")
save(doym.b, file="~/Documents/git/springfreeze/scripts/stan/dvr_sp_chill_realdata_issues.Rda")
# For Simon Joly:
range(sumerb[,"n_eff"])
summary(sumerb[,"n_eff"])
length(sumerb[(sumerb[,"n_eff"])<15988,"n_eff"])
length(sumerb[,"n_eff"])

#write.csv(sumerb, file="~/Documents/git/springfreeze/output/model_results.csv", row.names = FALSE)
# Also, I think AROMEL and ALNINC (Zohner spp) are 4-5 (double-check photoperiod effect)
# sumerb[grep("b_photo", rownames(sumerb)),]

# Below: Some pairs plots to check out
# pairs(doym.b, pars = c("mu_b_warm", "sigma_b_warm", "lp__"))
# These are very slow!
# pairs(doym.b, pars = c(names(doym.b)[grep("mu_b_inter", names(doym.b))],
#     names(doym.b)[grep("sigma_b_inter", names(doym.b))]))
# pairs(doym.b, pars = c(names(doym.b)[grep("mu_b_inter", names(doym.b))], "lp__"))
# pairs(doym.b, pars = c(names(doym.b)[grep("sigma_b_inter", names(doym.b))], "lp__"))


# save(doym.b, file="stan/output/lday_site_sp_chill_inter_poola_ncp_doymb.Rda")
# load('stan/lday_site_sp_chill_inter_poola_ncp_doymb.Rda')

# plot effects
col4fig <- c("mean","sd","25%","50%","75%","Rhat")
col4table <- c("mean","sd","2.5%","50%","97.5%","Rhat")

# manually to get right order
mu_params <- c("mu_b_warm","mu_b_photo","mu_b_chill1","mu_b_chill2",
               "mu_b_inter_wp",
               "mu_b_inter_wc1","mu_b_inter_wc2",
               "mu_b_inter_pc1","mu_b_inter_pc2")

meanzb <- sumerb[mu_params,col4fig]

rownames(meanzb) = c("Forcing Temperature",
                    "Photoperiod",
                    "Chilling 4°",
                    "Chilling 1.5°C",
                    "Forcing x Photoperiod",
                    "Forcing x Chilling 4°C",
                    "Forcing x Chilling 1.5°C",
                    "Photoperiod x Chilling 4°C",
                    "Photoperiod x Chilling 1.5°C"
                    )



meanzb.table <- sumerb[mu_params,col4table]
row.names(meanzb.table) <- row.names(meanzb)

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# ALERT: Fixing the 1/2 issue to 0/1 here
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
unique(dxl$warm)
dxl$warm[dxl$warm==1] <- 0
dxl$warm[dxl$warm==2] <- 1

unique(dxl$photo)
dxl$photo[dxl$photo==1] <- 0
dxl$photo[dxl$photo==2] <- 1

unique(dxl$chill1)
unique(dxl$chill2)

if(runstan){
  datalist.l <- list(lday = dxl$lday, # leaf-out as respose 
                     warm = as.numeric(dxl$warm), 
                     site = as.numeric(dxl$site), 
                     sp = as.numeric(dxl$sp), 
                     photo = as.numeric(dxl$photo), 
                     chill1 = as.numeric(dxl$chill1),
                     chill2 = as.numeric(dxl$chill2),
                     N = nrow(dxl), 
                     n_site = length(unique(dxl$site)), 
                     n_sp = length(unique(dxl$sp))
  )
  
    doym.l <- stan('stan/lday_site_sp_chill_inter_poola_ncp.stan',
                data = datalist.l, warmup=4000, iter = 7997, chains = 4,
                control = list(adapt_delta = 0.95))
                #               ,max_treedepth = 15))
}

#yl = dxl$lday # for shinystan posterior checks
# launch_shinystan(doym.l)

sumerl <- summary(doym.l)$summary
sumerl[grep("mu_", rownames(sumerl)),]

# For Simon:
range(sumerl[,"n_eff"])
summary(sumerl[,"n_eff"])
length(sumerl[(sumerl[,"n_eff"])<15988,"n_eff"])
length(sumerl[,"n_eff"])

# save(doym.l, file="stan/output/lday_site_sp_chill_inter_poola_ncp_doyl.Rda")

meanzl <- sumerl[mu_params,col4fig]
meanzl.table <- sumerl[mu_params,col4table]

rownames(meanzl) = rownames(meanzb)
rownames(meanzl.table) = rownames(meanzb.table)


gotchill <- tapply(dx$spn, dx$chill2, unique)$'1'
nochill <- unique(dx$spn)[is.na(match(unique(dx$spn), gotchill))]
sumerb[!is.na(match(rownames(sumerb), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerb[!is.na(match(rownames(sumerb), paste("b_chill2[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill1[", nochill, "]", sep=""))),] = NA
sumerl[!is.na(match(rownames(sumerl), paste("b_chill2[", nochill, "]", sep=""))),] = NA


## Look at posteriors of some ranefs (just one chain, that's the [[1]])
# hist(doym.b@sim$samples[[1]]$`b_photo[13]`, breaks=1000)
# hist(doym.l@sim$samples[[1]]$`b_photo[13]`, breaks=1000)

################
# Figure 1:
# Stan model effects for bud burst and leaf-out
################

## Want to flip the axes?
## Try source("source/figureflipping.R")
## It does the work for Fig. 1 .... would need more work to do for other figures 

bbpng <- readPNG(file.path(figpath, "Finn_BB.png")) # Illustrations from Finn et al.
lopng <- readPNG(file.path(figpath, "Finn_LO.png"))

pdf(file.path(figpath, "Fig1_bb_lo.pdf"), width = 7, height = 8)
  
  par(mfrow=c(2,1), mar = c(2, 10, 5, 1))
  
  # Upper panel: bud burst
  plot(seq(-22, 
           12,
           length.out = nrow(meanzb)), 
       1:nrow(meanzb),
       type="n",
       xlab = "",
       ylab = "",
       yaxt = "n")
  
  legend(x = -24, y = 6, bty="n", legend = "a. Budburst", text.font = 2)
  rasterImage(bbpng, -20, 1, -16, 4)
  
  axis(2, at = nrow(meanzb):1, labels = rownames(meanzb), las = 1, cex.axis = 0.8)
  points(meanzb[,'mean'],
         nrow(meanzb):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzb[,"75%"], nrow(meanzb):1, meanzb[,"25%"], nrow(meanzb):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)

  par(mar=c(5, 10, 2, 1))
  # Lower panel: leaf-out
  plot(seq(-22, 
           12, 
           length.out = nrow(meanzl)), 
       1:nrow(meanzl),
       type="n",
       xlab = "Model estimate change in day of phenological event",
       ylab = "",
       yaxt = "n")
  
  legend(x = -24, y = 6, bty="n", legend = "b. Leafout", text.font = 2)
  rasterImage(lopng, -20, 1, -14, 4)
  
  axis(2, at = nrow(meanzl):1, labels = rownames(meanzl), las = 1, cex.axis = 0.8)
  points(meanzl[,'mean'],
         nrow(meanzl):1,
         pch = 16,
         col = "midnightblue")
  arrows(meanzl[,"75%"], nrow(meanzl):1, meanzl[,"25%"], nrow(meanzl):1,
         len = 0, col = "black")
  abline(v = 0, lty = 3)
  
dev.off();#system(paste("open", file.path(figpath, "Fig1_bb_lo.pdf"), "-a /Applications/Preview.app"))

###############
# Figure 2: random effects.
# Photo x warm and chill1 x warm for bb and lo as 4 panels
###############
# Groups
colz = c("brown", "blue3")

shrubs = c("VIBLAN","RHAFRA","RHOPRI","SPIALB","VACMYR","VIBCAS", "AROMEL","ILEMUC", "KALANG", "LONCAN", "LYOLIG")
trees = c("ACEPEN", "ACERUB", "ACESAC", "ALNINC", "BETALL", "BETLEN", "BETPAP", "CORCOR", "FAGGRA", "FRANIG", "HAMVIR", "NYSSYL", "POPGRA", "PRUPEN", "QUEALB" , "QUERUB", "QUEVEL")

treeshrub = levels(dx$sp)
treeshrub[treeshrub %in% shrubs] = 1
treeshrub[treeshrub %in% trees] = 2
treeshrub = as.numeric(treeshrub)
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>


# Tip for checking figure range needs:
# range(sumerb[grep(paste("b_chill1","\\[",sep=""), rownames(sumerb)),1], na.rm=TRUE)
# Adjust sumerb -> sumerl and b_chill1 to other effects

pdf(file.path(figpath, "Fig2_4panel.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = T),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "DVR \n Change (days) due to 5° warming", font = 2, srt = 90) # \n\n add two line breaks

plotlet <- function(x, y, xlab=NULL, ylab=NULL, data, groups = NULL, ...){
  if(is.null(xlab)) xlab = x; if(is.null(ylab)) ylab = y
  if(is.null(groups)) { col.pch = "black"; col.lines = "grey50" }
  else {
    colz = c("brown", "blue3")
    ccolz = rep(colz[1], length(groups))
    ccolz[groups == 2] = colz[2]
    col.pch = ccolz
    col.lines = alpha(ccolz, 0.4)
  }
  
  plot(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
    pch = "+",
    ylab = ylab,
    xlab = xlab,
    col = col.pch,
    ...
  )
  
  abline(h=0, lty = 3, col = "grey60")
  abline(v=0, lty = 3, col = "grey60")
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"25%"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"75%"],
    length = 0, col = col.lines)
  
  arrows(
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"25%"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"],
    data[grep(paste(x,"\\[",sep=""), rownames(data)),"75%"],
    data[grep(paste(y,"\\[",sep=""), rownames(data)),"mean"],
    length = 0, col = col.lines)
  
  # match with species names
  text( data[grep(paste(x,"\\[",sep=""), rownames(data)),1],
        data[grep(paste(y,"\\[",sep=""), rownames(data)),1],
        sort(unique(dx$sp)),
        cex = 0.5, 
        pos = 3,
        col = col.pch)
}


plotlet( "photo", "force",
         #  ylab = "Advance due to 5° warming", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         #ylim = c(-27, 0.5),
         #xlim = c(-16, 0.5),
         #  xaxt="n", 
         group = treeshrub,
         data = fit1)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet("chill1", "force", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -8),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due to 5° warming", font = 2, srt = 90)

plotlet("photo", "force", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-27, 0.5),
        xlim = c(-16, 0.5),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)
plotlet("b_chill1", "b_warm", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -8),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)
plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 4 hr longer photoperiod", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 4° chilling", font = 2, pos = 3)

dev.off();#system(paste("open", file.path(figpath, "Fig2_4panel.pdf"), "-a /Applications/Preview.app"))


###############
# Figure 2* Supp version: random effects.
# Photo x warm and chill1 x warm for bb and lo as 4 panels
# Zoomed in and without the 50% credible intervals so you can see the species names
###############

pdf(file.path(figpath, "Fig2_4panel_ZoomSupp.pdf"), width = 7, height = 7)

par(mar=rep(1.25,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n", bty="n", xaxt="n", yaxt="n", ylab="", xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to 5° warming", font = 2, srt = 90)

plotlet.old( "b_photo", "b_warm",
         #  ylab = "Advance due to 5° warming", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-16, 0.5),
         xlim = c(-12, 0.5),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet.old("b_chill1", "b_warm", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -8),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = TRUE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due to 5° warming", font = 2, srt = 90) 

plotlet.old("b_photo", "b_warm", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-27, -12),
        xlim = c(-14, -7),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)
plotlet.old("b_chill1", "b_warm", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, -12),
        xlim = c(-28, -8),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = TRUE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)
plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 4 hr longer photoperiod", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 4° chilling", font = 2, pos = 3)

dev.off();


###############
# Figure CHILL2 for Supp (similar to Figure 2): random effects.
# photo x chill2 and chill2 x warm for bb and lo 
# Ailene wanted this (Ailene also wanted chill1 x photo, see below)
###############

pdf(file.path(figpath, "FigChill2_4panel.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Budburst \n Change (days) due to 5° warming", font = 2, srt = 90) # \n\n add two line breaks

plotlet("b_chill2", "b_warm",
         #  ylab = "Advance due to  chilling", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-27, 0.5),
         xlim = c(-28, -4),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet("b_chill2", "b_warm", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-27, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due 4 hr longer photoperiod", font = 2, srt = 90)

plotlet("b_chill2", "b_photo", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)

plotlet("b_chill2", "b_photo", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)

plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 30d 1.5° chilling", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 1.5° chilling", font = 2, pos = 3)

dev.off();



###############
# Figure CHILLxPHOTO for Supp (similar to Figure 2): random effects.
# photo x chill2 and photo x chill1 for bb and lo 
# Ailene wanted this also, but I am not including it in Supp until someone else wants it
###############

pdf(file.path(figpath, "FigChillPhoto_4panel.pdf"), width = 7, height = 7)

par(mar=rep(1,4))
layout(matrix(c(1, 2, 3, # use layout instead of par(mfrow for more control of where labels end up
                4, 5, 6,
                7, 8, 9),ncol = 3, byrow = TRUE),
       widths = c(1, 4, 4),
       heights = c(4, 4, 1))
plotblank = function(){plot(1:10, type="n",bty="n",xaxt="n",yaxt="n",ylab="",xlab="")}

plotblank() 
text(5,5, "Leafout \n Change (days) due 4 hr longer photoperiod", font = 2, srt = 90) # \n\n add two line breaks

plotlet("b_chill1", "b_photo",
         #  ylab = "Advance due to  chilling", 
         # xlab = "Advance due to 4 hr longer photoperiod", 
         ylim = c(-16, 0.5),
         xlim = c(-28, -4),
         #  xaxt="n", 
         group = treeshrub,
         data = sumerb)

legend("topleft", bty = "n", inset = 0.035, legend = "A.", text.font=2)

legend("bottomright",
       pch = "+",
       col = colz,
       legend = c("Shrubs","Trees"),
       inset = 0.02, 
       bg = 'white')

plotlet("b_chill2", "b_photo", 
        # ylab = "Advance due to 5° warming", 
        #  xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        # xaxt="n", 
        group = treeshrub,
        data = sumerb)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "B.", text.font=2)

plotblank()
text(5,5, "Leafout \n Change (days) due 4 hr longer photoperiod", font = 2, srt = 90)

plotlet("b_chill1", "b_photo", 
        #    ylab = "Advance due to 5° warming", 
        #     xlab = "Advance due to 4 hr longer photoperiod", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        group = treeshrub,
        data = sumerl)
legend("topleft", bty = "n", inset = 0.035, legend = "C.", text.font=2)

plotlet("b_chill2", "b_photo", 
        #   ylab = "Advance due to 5° warming", 
        #   xlab = "Advance due to 30d 4° chilling", 
        ylim = c(-16, 0.5),
        xlim = c(-28, -4),
        yaxt="n",
        group = treeshrub,
        data = sumerl)
axis(2, seq(0, -25, by = -5), labels = FALSE)
legend("topleft", bty = "n", inset = 0.035, legend = "D.", text.font=2)

plotblank()

plotblank()
text(5.5, 5, "Change (days) due to 30d 4° chilling", font = 2, pos = 3)

plotblank()
text(5.5, 5, "Change (days) due to 30d 1.5° chilling", font = 2, pos = 3)

dev.off();



# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>

if(runstan) { savestan("Inter") }

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# 2. Species-specific responses. Not using trait data here, just leaf-out and budburst vs warming and photoperiod. 

dlo <- summary(doym.l)$summary
dlo[!is.na(match(rownames(dlo), paste("b_chill1[", nochill, "]", sep=""))),] = 99
dlo[!is.na(match(rownames(dlo), paste("b_chill2[", nochill, "]", sep=""))),] = 99



# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# <><><><><><><><><><><><><><><><><><><><><><><><><><><><>
# Phylogeny
# 
# phsp <- ph$tip.label
# phspcode <- unlist(lapply(strsplit(phsp, "_"), function(x) toupper(paste(substr(x[[1]],1,3), substr(x[[2]],1,3), sep=""))))
# 
# ph$tip.label = phspcode
# 
# ph$node.label = NULL # otherwise give duplicated names error, because of multiple "" in node labels.
# 
# sig <- comparative.data(ph, dxt2, names.col = "sp")
# 
# lo.signal.warm <- pgls(lo ~ warm, sig, lambda = 'ML')
# lo.signal.photo <- pgls(lo ~ photoeff, sig, lambda = 'ML')
# lo.signal.chill1 <- pgls(lo ~ chill1eff, sig, lambda = 'ML')
# lo.signal.chill2 <- pgls(lo ~ chill2eff, sig, lambda = 'ML')
# 
# bb.signal.warm <- pgls(bb ~ warmeff, sig, lambda = 'ML')
# bb.signal.photo <- pgls(bb ~ photoeff, sig, lambda = 'ML')
# bb.signal.chill1 <- pgls(bb ~ chill1eff, sig, lambda = 'ML')
# bb.signal.chill2 <- pgls(bb ~ chill2eff, sig, lambda = 'ML')
# 
# 
# signaldat <- data.frame(
#   rbind(summary(bb.signal.warm)$param["lambda"], 
#         summary(bb.signal.photo)$param["lambda"],
#         summary(bb.signal.chill1)$param["lambda"],
#         summary(bb.signal.chill2)$param["lambda"],
#         
#         summary(lo.signal.warm)$param["lambda"], 
#         summary(lo.signal.photo)$param["lambda"],
#         summary(lo.signal.chill1)$param["lambda"],
#         summary(lo.signal.chill2)$param["lambda"]
# ))
#         
# signaldat$var = paste(
#   rep(c("Bud burst","Leaf-out"), each = 4), 
#   rep(c("Forcing", "Photoperiod", "Chilling 4°", "Chilling 1.5°"), 4), 
#   sep = " - ")
# 
# 
# phylosigtable <- xtable(data.frame(Relationship = signaldat[,"var"],Lambda = signaldat[,"lambda"]), digits = 3,
#                         caption = "Phylogenetic signal in timing of bud burst and leaf-out and species specific traits, as estimated in the caper package with simultaneous fitting of lambda.  Pore anatomy (ring- versus diffuse-porous species) was highly clustered phylogenetically, but no other trait examined demonstrated significant phylogenetic signal")
# 
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Plot actual change in leaf-out by species, for Figure 1
lday.agg <- aggregate(dx$lday, by=list(sp=dx$sp,
                                       warm=dx$warm, photo=dx$photo
                                       #,chill=dx$chill, site=dx$site,  treatcode=dx$treatcode, 
                                       ), FUN = mean, na.rm=T)

lday.se <- aggregate(dx$lday, list(sp=dx$sp, 
                                   warm=dx$warm, photo=dx$photo
                                   #,chill=dx$chill, site=dx$site, treatcode=dx$treatcode, 
                                   ), function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)])))
lday.agg$se = lday.se$x

# Advance due to each factor
wa = la = ca = oa = nn = vector()
wab = lab = cab = oab = vector()

for(i in unique(dx$sp)){ # i="ACEPEN"
  dxx <- dx[dx$sp == i,]
  
  nn <- c(nn, nrow(dxx[dxx$nl==1,]))
  
  overallm = mean(dxx[dxx$warm == 1 & dxx$photo == 1 & dxx$chill == 1, "lday"], na.rm=T)
  overallmb = mean(dxx[dxx$warm == 1 & dxx$photo == 1 & dxx$chill == 1, "bday"], na.rm=T)
  
  # mean across all cool
  cm <- mean(dxx[dxx$warm == 1,'lday'], na.rm=T)
  cmb <- mean(dxx[dxx$warm == 1,'bday'], na.rm=T)
  
  # advance from warming
  wm <- mean(dxx[dxx$warm == 2, 'lday'], na.rm=T)
  wmb <- mean(dxx[dxx$warm == 2, 'bday'], na.rm=T)
  warmadv = wm - cm    
  warmadvb = wmb - cmb
  
  # mean across all short
  sm <- mean(dxx[dxx$photo == 1,'lday'], na.rm=T)
  smb <- mean(dxx[dxx$photo == 1,'bday'], na.rm=T)
  
  # mean across long
  lm <- mean(dxx[dxx$photo == 2, 'lday'], na.rm=T)
  lmb <- mean(dxx[dxx$photo == 2, 'bday'], na.rm=T)
  
  # advance from photo
  longadv = lm - sm   
  longadvb = lmb - smb   
  
  # mean across chill1 (no additional chill)
  cm <- mean(dxx[dxx$chill == 1,'lday'], na.rm=T)
  cmb <- mean(dxx[dxx$chill == 1,'bday'], na.rm=T)

  # mean across chill2 (chill 4deg)
  wm <- mean(dxx[dxx$chill == 2, 'lday'], na.rm=T)
  wmb <- mean(dxx[dxx$chill == 2, 'bday'], na.rm=T)
  chilladv = wm - cm    
  chilladvb = wmb - cmb
  
  # advance from chill
  
  wa = c(wa, warmadv); la =c(la, longadv); oa=c(oa, overallm); ca = c(ca, chilladv)
  wab = c(wab, warmadvb); lab =c(lab, longadvb); oab=c(oab, overallmb); cab = c(cab, chilladvb)
  }
adv=data.frame(sp=unique(dx$sp), warm=wa, photo=la, overall=oa, n=nn, chill = ca,
               warmb=wab, photob=lab, overallb=oab, chillb = cab)

# Color classes for early - mid - late fl
#hist(adv$overall)
oa.col <- cut(adv$overall, 3)
levels(oa.col) <- oc <- alpha(c("blue","purple","red"), 0.8)
oa.col = as.character(oa.col)

pdf(file.path(figpath, "Advplot2.pdf"), width = 8, height = 9) # Adv plot is without varying cex by sample size
plot(adv$photo, adv$warm, 
     ylim = c(-30, -2),
     xlim=c(-20,-2),
     xlab = "Advance in leafout due to 4h longer photoperiod",
     ylab = "Advance in leafout due to 5°C warmer temperature",
     pch = 16, 
     col = oa.col,
     cex = adv$n/12#log(adv$n)
     )

text(adv$photo, adv$warm,
     labels = adv$sp, cex = 0.7, adj = 0.5, #pos = 3,
     col = alpha('grey20', 0.9))

legend(x = -20, y = -3, bty = "n", legend = c("Early", "Intermediate", "Late"), title = "Leafout period", col = oc, pch = 16, 
       pt.cex = 2,
       y.intersp = 1.5,
       x.intersp = 1.5)
legend(x = -16, y = -3, bty = "n", legend = c(25, 75), 
       title = "N leafout samples", col = "black", pch = 1, pt.cex = c(25, 75)/12,
       y.intersp = 2,
       x.intersp = 2
)

dev.off();#system(paste("open", file.path(figpath, "Advplot2.pdf"), "-a /Applications/Preview.app"))

## Correlations between main effects and lo/bb
# warm, photo, chill1, chill2 vs. day of lo and day of bb

#bb, warm
bwarm <- sumerb[grep(paste("b_warm","\\[",sep=""), rownames(sumerb)),1]
bphoto <- sumerb[grep(paste("b_photo","\\[",sep=""), rownames(sumerb)),1]
bchill1 <- sumerb[grep(paste("b_chill1","\\[",sep=""), rownames(sumerb)),1]

lwarm <- sumerl[grep(paste("b_warm","\\[",sep=""), rownames(sumerl)),1]
lphoto <- sumerl[grep(paste("b_photo","\\[",sep=""), rownames(sumerl)),1]
lchill1 <- sumerl[grep(paste("b_chill1","\\[",sep=""), rownames(sumerl)),1]

pdf(file.path(figpath, "Sens_vs_day.pdf"), width = 9, height = 7)

par(mfrow=c(2,3))
plot(adv$overallb, bwarm, ylab = "Warming sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of budburst")
# legend("top", legend="Budburst", text.font=2, inset = 0.05, bty ="n", cex = 2)
plot(adv$overallb, bphoto, ylab = "Photoperiod sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of budburst")
plot(adv$overallb, bchill1, #ylim = c(-30, -10), 
     ylab = "Chilling sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of budburst")

plot(adv$overall, lwarm, ylab = "Warming sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leafout")
#legend("top", legend="Leafout", text.font=2, inset = 0.05, bty ="n", cex = 2)
plot(adv$overall, lphoto, ylab = "Photoperiod sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leafout")
plot(adv$overall, lchill1, #  ylim = c(-30, -10), 
     ylab = "Chilling sensitivity", pch = 16, cex = 2, col = alpha("grey20", 0.6), xlab = "Day of leafout")

dev.off();#system(paste("open", file.path(figpath, "Sens_vs_day.pdf"), "-a /Applications/Preview.app"))

##
## Added by Lizzie on 30 July 2017 for ESA talk
## Just doing leafout for now
##

df.lsens <- data.frame(sp=adv$sp, lwarm=lwarm, lphoto=lphoto, lchill1=lchill1, group=treeshrub,
    overall=adv$overall)

pdf(file.path(figpath, "Sens_vs_day_treeshrub.pdf"), width = 9, height = 3.5)

par(mfrow=c(1,3))
plot(lwarm~overall, ylab = "Warming sensitivity", pch = 16, cex = 2, col = alpha("firebrick3", 0.6), xlab = "Day of leafout",
    data=subset(df.lsens, group==1), ylim=c(-28,-11), xlim=c(20,90))
points(lwarm~overall, pch = 16, cex = 2, col = alpha("blue3", 0.6), data=subset(df.lsens, group==2),
    ylim=c(-28,-11), xlim=c(20,90))

plot(lphoto~overall, ylab = "Photoperiod sensitivity", pch = 16, cex = 2, col = alpha("firebrick3", 0.6), xlab = "Day of leafout",
    data=subset(df.lsens, group==1), ylim=c(-14,-7), xlim=c(20,90))
points(lphoto~overall, pch = 16, cex = 2, col = alpha("blue3", 0.6), data=subset(df.lsens, group==2),
    ylim=c(-14,-7), xlim=c(20,90))

plot(lchill1~overall, ylab = "Chilling sensitivity", pch = 16, cex = 2, col = alpha("firebrick3", 0.6), xlab = "Day of leafout",
    data=subset(df.lsens, group==1), ylim=c(-28,-11), xlim=c(20,90))
points(lchill1~overall, pch = 16, cex = 2, col = alpha("blue3", 0.6), data=subset(df.lsens, group==2),
    ylim=c(-28,-11), xlim=c(20,90))

dev.off();#system(paste("open", file.path(figpath, "Sens_vs_day.pdf"), "-a /Applications/Preview.app"))


on.exit(setwd("~/Documents/git/projects/treegarden/budexperiments/docs/ms"))
