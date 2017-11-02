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
d<- data.frame(study=c("Lenz2016: Sorbus aucuparia (50%)", "Lenz2016: Prunus avium (50%)", "Lenz2016: Tilia platyphyllos (50%)", 
                       "Lenz2016: Acer pseudoplatanus (50%)", "Lenz2016: Fagus sylvatica (50%)", "Schwartz1993: All species (hard)", 
                       "Augspurger2013: All species (soft)", "Barker2005: Eucalyptus pauciflora", "Peterson2014: All species",
                       "Cannell1986: All species", "Longstroth2012: Vaccinium spp.", "Longstroth2013: Rosaceae (10%)", "Longstroth2013: Rosaceae (90%)",
                       "Barlow2015: Wheat (10-90%)", "Barlow2015: Wheat (100%)", "Sanchez2013: Rice (100%)", "Sanchez2013: Corn (100%)", "Sanchez: Wheat (100%)"), 
               phase=c("Vegetative", "Vegetative", "Vegetative", "Vegetative", "Vegetative", "Both", "Both", "Both", "Both", "Both",
                       "Floral", "Vegetative", "Vegetative", "Floral", "Vegetative", "Vegetative", "Vegetative", "Vegetative"),
               sector=c("Ecologicial", "Ecologicial", "Ecologicial", "Ecologicial", "Ecologicial", "Ecologicial",
                        "Ecologicial", "Ecologicial", "Ecologicial", "Agrinomic", "Agrinomic", "Agrinomic", "Agrinomic",
                        "Agrinomic", "Agrinomic", "Agrinomic", "Agrinomic", "Agrinomic"),
               temperature=c(-7.4, -8.5, -7.4, -6.7, -4.8, -2.2, -1.7, -5.8, -2.2, 2, -2.2, -7.2, -13.3, -4.5,
                             -5.5, 4.7, -1.8, -17.2),
               sd=c(4, 5, 3, 4, 3, 0, 0, 0, 0, 1, 2.2, 0, 0, 0.5, 1.5, 1.3, 1.9, 1.2))

d$alph<-ifelse(d$sector=="Agrinomic", 1, 2)
d$set<-reorder(d$study, d$alph)
ggplot(d, aes(x=set, y=temperature)) + 
         geom_linerange(aes(ymin=temperature-sd, ymax=temperature+sd, color=sector), alpha=0.3) + 
         geom_point(aes(shape=phase, color=sector)) + ylab("Temperature Threshold") +
  xlab("Study, Taxonomic group, Freeze Definition") + 
  geom_hline(yintercept=0, linetype=2) + coord_flip() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
