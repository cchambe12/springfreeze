### Make a new chart that shows the frost hardiness before, during and after dormancy
## Hardiness graph
# 8 February 2018 - Cat

# Load Libraries
library(ggplot2)

month<- 1:9
hrdy<-data.frame(month, hardy=NA, phase=NA)
hrdy$hardy<-c(-8, -12, -18, -25, -30, -25, -10, -2, -3)
hrdy$phase<-c("Paradormancy","Paradormancy", "Endodormancy", "Endodormancy", "Endodormancy", "Ecodormancy",
              "Ecodormancy", "Budburst", "Leaf Expansion")

ggplot(hrdy, aes(x=month, y=hardy)) +   
  scale_x_continuous(position = "top", breaks=c(1,2,3,4,5,6,7,8, 9),
                     labels=c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")) +
  geom_point(aes(col=phase), col=c("sienna4","sienna4", "purple4", "purple4", "purple4", "navyblue", "navyblue", "green4", "yellowgreen")) +
  geom_curve(aes(x = 1, y = -8, xend = 2, yend = -12), data = hrdy, col="sienna4", curvature =0.02) +
  geom_curve(aes(x = 2, y = -12, xend = 3, yend = -18), data = hrdy, col="sienna4", curvature =-0.02) +
  geom_curve(aes(x = 3, y = -18, xend = 4, yend = -25), data = hrdy, col="purple3", curvature = -0.02) +
  geom_curve(aes(x = 4, y = -25, xend = 5, yend = -30), data = hrdy, col="purple3", curvature = 0.05) +
  geom_curve(aes(x = 5, y = -30, xend = 6, yend = -25), data = hrdy, col="purple3", curvature = 0.05) +
  geom_curve(aes(x = 6, y = -25, xend = 7, yend = -10), data = hrdy, col="navyblue", curvature = 0.02) +
  geom_curve(aes(x = 7, y = -10, xend = 8, yend = -2), data = hrdy, col="green4", curvature = -0.1) +
  geom_curve(aes(x = 8, y = -2, xend = 9, yend = -3), data = hrdy, col="yellowgreen", curvature = -0.1) +
  scale_y_continuous(breaks=c(2, 0, -5, -10, -15, -20, -25, -30, -35)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Month") + ylab("Cold Hardiness") + 
  annotate("text", x = 2.25, y = -11.5, label = "Paradormancy", angle=318, size=3) +
  annotate("text", x = 4.15, y = -24, label = "Endodormancy", angle=310, size=3) +
  annotate("text", x = 6.45, y = -15.35, label = "Ecodormancy", angle=70, size=3) +
  annotate("text", x = 8, y = -1, label = "Budburst", size=3) +
  annotate("text", x = 9, y = -4, label = "Leafout", size=3)

ggplot(hrdy, aes(x=month, y=hardy)) +   
  scale_x_continuous(position = "top", breaks=c(1,2,3,4,5,6,7,8, 9),
                     labels=c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")) +
  geom_point(aes(col=phase)) +
  geom_curve(aes(x = 1, y = -8, xend = 2, yend = -12), data = hrdy, col="sienna4", curvature =0.02) +
  geom_curve(aes(x = 2, y = -12, xend = 3, yend = -18), data = hrdy,col="sienna4", curvature =-0.02) +
  geom_curve(aes(x = 3, y = -18, xend = 4, yend = -25), data = hrdy,col="purple3", curvature = -0.02) +
  geom_curve(aes(x = 4, y = -25, xend = 5, yend = -30), data = hrdy,col="purple3", curvature = 0.05) +
  geom_curve(aes(x = 5, y = -30, xend = 6, yend = -25), data = hrdy,col="purple3", curvature = 0.05) +
  geom_curve(aes(x = 6, y = -25, xend = 7, yend = -10), data = hrdy,col="navyblue", curvature = 0.02) +
  geom_curve(aes(x = 7, y = -10, xend = 8, yend = -2), data = hrdy,col="green4", curvature = -0.1) +
  geom_curve(aes(x = 8, y = -2, xend = 9, yend = -3), data = hrdy,col="yellowgreen", curvature = -0.1) +
  scale_y_continuous(breaks=c(2, 0, -5, -10, -15, -20, -25, -30, -35)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
        legend.key=element_rect(fill="white")) +
  xlab("Month") + ylab("Cold Hardiness") + 
  scale_color_manual(values=c("green4", "purple4", "navyblue","yellowgreen" , "sienna4"),
                     labels=c("Budburst", "Endodormancy", "Ecodormancy","Leaf Expansion","Paradormancy")) +
  labs(color="Phenophase")

ggplot(hrdy, aes(x=month, y=hardy)) +   
  scale_x_continuous(position = "top", breaks=c(1,2,3,4,5,6,7,8, 9),
                     labels=c("Sept", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar", "Apr", "May")) +
  geom_curve(aes(x = 1, y = -8, xend = 2, yend = -12), data = hrdy, curvature =0.02) +
  geom_curve(aes(x = 2, y = -12, xend = 3, yend = -18), data = hrdy, curvature =-0.02) +
  geom_curve(aes(x = 3, y = -18, xend = 4, yend = -25), data = hrdy, curvature = -0.02) +
  geom_curve(aes(x = 4, y = -25, xend = 5, yend = -30), data = hrdy, curvature = 0.05) +
  geom_curve(aes(x = 5, y = -30, xend = 6, yend = -25), data = hrdy, curvature = 0.05) +
  geom_curve(aes(x = 6, y = -25, xend = 7, yend = -10), data = hrdy, curvature = 0.02) +
  geom_curve(aes(x = 7, y = -10, xend = 8, yend = -2), data = hrdy, curvature = -0.1) +
  geom_curve(aes(x = 8, y = -2, xend = 9, yend = -3), data = hrdy, curvature = -0.1) +
  scale_y_continuous(breaks=c(2, 0, -5, -10, -15, -20, -25, -30, -35)) +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  xlab("Month") + ylab("Cold Hardiness")

