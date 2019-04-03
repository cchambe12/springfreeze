## Updating format of Temperature Table

## 30 October 2017

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(ggplot2)

## Let's make the data...
d<- data.frame(study=c("Sorbus aucuparia - 50% (Lenz et al., 2016)", "Prunus avium - 50% (Lenz et al., 2016)", "Tilia platyphyllos - 50% (Lenz et al., 2016)", 
                       "Acer pseudoplatanus - 50% (Lenz et al., 2016)", "Fagus sylvatica - 50% (Lenz et al., 2016)", "All species - hard freeze (Schwartz, 1993))", 
                       "All species - soft freeze (Augspurger, 2013)", "Eucalyptus pauciflora (Barker et al., 2005)", "All species (Peterson & Abatzoglou, 2014)",
                       "All species (Cannell & Smith, 1986)", "Vaccinium spp. (Longstroth, 2012)", "Rosaceae - 10% (Longstroth, 2013)", "Rosaceae - 90% (Longstroth, 2013)",
                       "Wheat - 10 to 90% (Barlow et al., 2015)", "Wheat - 100% (Barlow et al., 2015)", "Rice - 100% (Sanchez et al., 2013)", "Corn - 100% (Sanchez et al., 2013)", "Wheat - 100% (Sanchez et al., 2013)"), 
               Phase=c("Vegetative", "Vegetative", "Vegetative", "Vegetative", "Vegetative", "Both", "Both", "Both", "Both", "Both",
                       "Floral", "Vegetative", "Vegetative", "Floral", "Vegetative", "Vegetative", "Vegetative", "Vegetative"),
               Sector=c("Ecological", "Agronomic", "Ecological", "Ecological", "Ecological", "Ecological",
                        "Ecological", "Ecological", "Ecological", "Agronomic", "Agronomic", "Agronomic", "Agronomic",
                        "Agronomic", "Agronomic", "Agronomic", "Agronomic", "Agronomic"),
               temperature=c(-7.4, -8.5, -7.4, -6.7, -4.8, -2.2, -1.7, -5.8, -2.2, 2, -2.2, -7.2, -13.3, -4.5,
                             -5.5, 4.7, -1.8, -17.2),
               sd=c(4, 5, 3, 4, 3, 0, 0, 0, 0, 1, 2.2, 0, 0, 0.5, 1.5, 1.3, 1.9, 1.2))


d$alph<-ifelse(d$Sector=="Agronomic", 1, 2)
d$dataset<-reorder(d$study, d$alph)
d$dataset <- factor(d$dataset, levels = d$dataset[order(d$Sector, d$temperature)])
my.xlab = expression(paste("Temperature Threshold ", degree,"C"))
temp<-ggplot(d, aes(x=dataset, y=temperature, col=Sector)) + 
  geom_linerange(aes(ymin=temperature-sd, ymax=temperature+sd), alpha=0.3) + 
  geom_point(aes(shape=Phase)) + ylab(my.xlab) + theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0, linetype=2) + geom_vline(xintercept = 10.5, linetype=4) +
  annotate("text", y=-15, x= 10, label="Agronomic", family="Helvetica", size=3, fontface="bold") +
  annotate("text", y=-15, x= 18, label="Ecological", family="Helvetica", size=3, fontface="bold") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"), legend.key=element_blank(), plot.title = element_text(family="Helvetica")) + 
  ylim(c(-20, 10))

temp<- temp + scale_x_discrete(labels=c("All species - soft freeze (Augspurger, 2013)"="All species - soft freeze (Augspurger, 2013)",
                                        "All species (Peterson & Abatzoglou, 2014)"="All species (Peterson & Abatzoglou, 2014)",
                                        "All species - hard freeze (Schwartz, 1993))"="All species - hard freeze (Schwartz, 1993))",
                                        "Fagus sylvatica - 50% (Lenz et al., 2016)" = expression(paste(italic("Fagus sylvatica"),  "- 50% (Lenz et al., 2016)")),
                                        "Eucalyptus pauciflora (Barker et al., 2005)"=expression(paste(italic("Eucalyptus pauciflora"), "(Barker et al., 2005)")),
                                        "Acer pseudoplatanus - 50% (Lenz et al., 2016)"=expression(paste(italic("Acer pseudoplatanus"), "- 50% (Lenz et al., 2016)")),
                                        "Tilia platyphyllos - 50% (Lenz et al., 2016)"=expression(paste(italic("Tilia platyphyllos"), "- 50% (Lenz et al., 2016)")),
                                        "Sorbus aucuparia - 50% (Lenz et al., 2016"=expression(paste(italic("Sorbus aucuparia"), "- 50% (Lenz et al., 2016")),
                                        "Prunus avium - 50% (Lenz et al., 2016)"=expression(paste(italic("Prunus avium"), "- 50% (Lenz et al., 2016)")),
                                        "Rice - 100% (Sanchez et al., 2013)"="Rice - 100% (Sanchez et al., 2013)",
                                        "All species (Cannell & Smith, 1986)"="All species (Cannell & Smith, 1986)",
                                        "Corn - 100% (Sanchez et al., 2013)"="Corn - 100% (Sanchez et al., 2013)",
                                        "Vaccinium spp. (Longstroth, 2012)"=expression(paste(italic("Vaccinium spp."), "(Longstroth, 2012)")),
                                        "Wheat - 10 to 90% (Barlow et al., 2015)"="Wheat - 10 to 90% (Barlow et al., 2015)",
                                        "Wheat - 100% (Barlow et al., 2015)"="Wheat - 100% (Barlow et al., 2015)",
                                        "Rosaceae - 10% (Longstroth, 2013)"=expression(paste(italic("Rosaceae"), "- 10% (Longstroth, 2013)")),
                                        "Rosaceae - 90% (Longstroth, 2013)"=expression(paste(italic("Rosaceae"), "- 90% (Longstroth, 2013)")),
                                        "Wheat - 100% (Sanchez et al., 2013)"="Wheat - 100% (Sanchez et al., 2013)"))
temp<-temp +coord_flip() #+ ggtitle("Descrepancies in Defining \nFalse Spring Temperatures")
quartz()
plot(temp)


