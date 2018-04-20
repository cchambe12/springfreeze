### Make a new chart that shows the false spring risk across species
## Avoidance graph
# 15 September 2017 - Cat

# Load Libraries
library(ggplot2)
library(gridExtra)

doy<- 75:150
ile<-data.frame(doy, phase=NA, risk=NA)
ile$phase<-ifelse(ile$doy==95, "BB", ile$phase)
ile$phase<-ifelse(ile$doy==100, "LO", ile$phase)
ile$risk<-ifelse(ile$doy>=96 & ile$doy<=99, 10, ile$risk)
ile$risk<-ifelse(ile$doy==95, 6.5, ile$risk)
ile$risk<-ifelse(ile$doy==100, 7, ile$risk)
ile$risk<-ifelse(ile$doy>=75 & ile$doy<=94, 3, ile$risk)
ile$risk<-ifelse(ile$doy>=101, 4, ile$risk)
write.csv(ile, file = "~/Documents/git/springfreeze/output/ilemuc.csv", row.names = FALSE)

bet<-data.frame(doy, phase=NA, risk=NA)
bet$phase<-ifelse(bet$doy==122, "BB", bet$phase)
bet$phase<-ifelse(bet$doy==142, "LO", bet$phase)
bet$risk<-ifelse(bet$doy>=124 & bet$doy<=140, 10, bet$risk)
bet$risk<-ifelse(bet$doy==123, 6.5, bet$risk)
bet$risk<-ifelse(bet$doy==141, 7, bet$risk)
bet$risk<-ifelse(bet$doy>=75 & bet$doy<=122, 3, bet$risk)
bet$risk<-ifelse(bet$doy>=142, 4, bet$risk)
write.csv(bet, file = "~/Documents/git/springfreeze/output/betall.csv", row.names = FALSE)

ilemuc<-ggplot(ile, aes(x=doy, y=risk)) + geom_line() + coord_cartesian(ylim=0:20) + 
  annotate("rect", xmin=82, xmax=97, ymin=0.5
           , ymax=6, alpha=0.1, color="red") +
  xlab("Day of Year") + ylab("Frost Damage Risk") +
  annotate("text", x = 140, y = 18, label = "Ilex mucronata", fontface = "italic") +
  annotate("text", x = 90, y = 4.5, label = "False", fontface="bold") +
  annotate("text", x = 90, y = 2.05, label = "Spring", fontface="bold") +
  annotate("text", x = 97.5, y = 11.5, label = "Duration of Vegetative Risk") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                                      panel.background = element_blank(), 
                                                                                      axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank()) + scale_y_discrete(limits=c("", "low",  "", "", "", "", "", "", "", "", "", "", "", "", "", "", "high"))+ 
  annotate("segment", x = 96, xend = 96, y = 16, yend = 10, colour = "lightgreen", arrow=arrow(length=(unit(0.3, "cm")))) +
  annotate("segment", x = 99, xend = 99, y = 16, yend = 10, colour = "darkgreen", arrow=arrow(length=(unit(0.3, "cm"))))

betall<-ggplot(bet, aes(x=doy, y=risk)) + geom_line() + coord_cartesian(ylim=0:20) + 
  annotate("rect", xmin=82, xmax=97, ymin=0.5, ymax=6, alpha=0.1, color="red") +
  xlab("Day of Year") + ylab("Frost Damage Risk") +
  annotate("text", x = 140, y = 18, label = "Betula alleghaniensis", fontface = "italic") +
  annotate("text", x = 90, y = 4.5, label = "False", fontface="bold") +
  annotate("text", x = 90, y = 2.05, label = "Spring", fontface="bold") +
  annotate("text", x = 132.5, y = 11.5, label = "Duration of Vegetative Risk") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                                       panel.background = element_blank(), 
                                                                                       axis.line = element_line(colour = "black"), axis.ticks.y = element_blank(), legend.key=element_blank()) + scale_y_discrete(limits=c("","low", "", "", "", "", "", "", "", "", "", "", "", "", "", "", "high"))+ 
  annotate("segment", x = 124, xend = 124, y = 16, yend = 10, colour = "lightgreen", arrow=arrow(length=(unit(0.3, "cm")))) +
  annotate("segment", x = 140, xend = 140, y = 16, yend = 10, colour = "darkgreen", arrow=arrow(length=(unit(0.3, "cm"))))
  
grid.arrange(ilemuc, betall, ncol=1, nrow=2)
