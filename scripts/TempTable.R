## Updating format of Temperature Table

## 30 October 2017
## Attempt to create a timeline chart
# Using John O'Keefe's Data!!

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

## Let's make the data...
d<- data.frame(study=c("Sorbus aucuparia - 50% (Lenz et al., 2016)", "Prunus avium - 50% (Lenz et al., 2016)", "Tilia platyphyllos - 50% (Lenz et al., 2016)", 
                       "Acer pseudoplatanus - 50% (Lenz et al., 2016)", "Fagus sylvatica - 50% (Lenz et al., 2016)", "All species - hard freeze (Schwartz, 1993))", 
                       "All species - soft freeze (Augspurger, 2013)", "Eucalyptus pauciflora (Barker et al., 2005)", "All species (Peterson & Abatzoglou, 2014)",
                       "All species (Cannell & Smith, 1986)", "Vaccinium spp. (Longstroth, 2012)", "Rosaceae - 10% (Longstroth, 2013)", "Rosaceae - 90% (Longstroth, 2013)",
                       "Wheat - 10 to 90% (Barlow et al., 2015)", "Wheat - 100% (Barlow et al., 2015)", "Rice - 100% (Sanchez et al., 2013)", "Corn - 100% (Sanchez et al., 2013)", "Wheat - 100% (Sanchez et al., 2013)"), 
               phase=c("Vegetative", "Vegetative", "Vegetative", "Vegetative", "Vegetative", "Both", "Both", "Both", "Both", "Both",
                       "Floral", "Vegetative", "Vegetative", "Floral", "Vegetative", "Vegetative", "Vegetative", "Vegetative"),
               sector=c("Ecologicial", "Ecologicial", "Ecologicial", "Ecologicial", "Ecologicial", "Ecologicial",
                        "Ecologicial", "Ecologicial", "Ecologicial", "Agronomic", "Agronomic", "Agronomic", "Agronomic",
                        "Agronomic", "Agronomic", "Agronomic", "Agronomic", "Agronomic"),
               temperature=c(-7.4, -8.5, -7.4, -6.7, -4.8, -2.2, -1.7, -5.8, -2.2, 2, -2.2, -7.2, -13.3, -4.5,
                             -5.5, 4.7, -1.8, -17.2),
               sd=c(4, 5, 3, 4, 3, 0, 0, 0, 0, 1, 2.2, 0, 0, 0.5, 1.5, 1.3, 1.9, 1.2))

d$alph<-ifelse(d$sector=="Agronomic", 1, 2)
d$dataset<-reorder(d$study, d$alph)
d$dataset <- factor(d$dataset, levels = d$dataset[order(d$sector, d$temperature)])
my.xlab = expression(paste("Temperature Threshold ", degree,"C"))
ggplot(d, aes(x=dataset, y=temperature)) + 
         geom_linerange(aes(ymin=temperature-sd, ymax=temperature+sd, color=sector), alpha=0.3) + 
         geom_point(aes(shape=phase, color=sector)) + ylab(my.xlab) + theme(axis.title.y=element_blank()) +
  geom_hline(yintercept=0, linetype=2) + coord_flip() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  ylim(c(-20, 10))
