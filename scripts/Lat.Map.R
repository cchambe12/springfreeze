## 30 December 2016 - Cat
## Attempt to Map out Latitudinal Data and FS

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Install Packages
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(tidyr)
library(mapproj)
library(grid)
library(rworldmap)


# Upload US map
usa <- map_data("usa")
states <- map_data("state")
canada<-map("worldHires","Canada", xlim=c(-141,-53), ylim=c(40,85), col="gray90", fill=TRUE)

gg1<- NAmap <- ggplot() + geom_polygon(data = usa, 
                                       aes(x=long, y = lat, group = group), 
                                       fill = "white", 
                                       color="black") +
  geom_polygon(data = states, aes(x=long, y = lat, group = group), 
               fill = "white", color="grey") + geom_polygon(data = canada, 
                                                            aes(x=long, y = lat, group = group), 
                                                            fill = "white", 
                                                            color="grey")

# Upload Data
usa<-read.csv("~/Documents/git/springfreeze/input/america.lat.csv", header=TRUE)
europe<-read.csv("~/Documents/git/springfreeze/input/europe.lat.csv", header=TRUE)
mich<-read.csv("~/Documents/git/springfreeze/input/mich.lat.csv", header=TRUE)
west<-read.csv("~/Documents/git/springfreeze/input/west.lat.csv", header=TRUE)

# USA Map
gg1 + 
  geom_point(data = usa, aes(x = Longitude, y = Latitude), color = "black", size = 3) +
  geom_point(data = usa, aes(x = Longitude, y = Latitude), size = 3) + 
  geom_point(fill=factor(usa$False.Springs)) + theme(legend.position="none")

am.map <- gg1 + geom_point(data = usa, aes(Longitude, Latitude, size=hf.gdd,color=hf.gdd)) +
  scale_color_gradient(low="red", high="blue", name="Number of False Springs") + theme(legend.position="none") +
  guides(size=FALSE)
am.map1<- am.map + geom_point(data = mich, aes(Longitude, Latitude, size=hf.gdd,color=hf.gdd)) +
  scale_color_gradient(low="red", high="blue", name="Number of False Springs") + theme(legend.position="none") +
  guides(size=FALSE)
am.map2<- am.map1 + geom_point(data = west, aes(Longitude, Latitude, size=hf.gdd,color=hf.gdd)) +
  scale_color_gradient(low="red", high="blue", name="Number of False Springs") + theme(legend.position="none") +
  guides(size=FALSE)

# Europe Map
# Get the world map
worldMap <- getMap()

# European Countries
europeanUnion <- c("Austria","Belgium","Bulgaria","Croatia","Cyprus",
                   "Czech Rep.","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Netherlands","Norway","Poland",
                   "Portugal","Romania","Slovakia","Slovenia","Spain",
                   "Sweden","Switzerland", "United Kingdom")
indEU <- which(worldMap$NAME%in%europeanUnion)
europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)

eur <- ggplot(europeCoords) + geom_polygon(data = europeCoords, aes(x = long, y = lat, group=region), 
                                           color="grey", fill="white") + coord_map(xlim = c(-13, 35),  ylim = c(32, 71))

eur.map <- eur + geom_point(data = europe, aes(Longitude, Latitude, size=hf.gdd, color=hf.gdd)) + 
  scale_color_gradient(low="red", high="blue", name="Number of False Springs")  + theme(legend.position="none") +
  guides(size=FALSE)
  
 

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
    
  combined <- switch(position,
                       "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                              legend,
                                              ncol = 1,
                                              heights = unit.c(unit(1, "npc") - lheight, lheight)),
                       "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                             legend,
                                             ncol = 2,
                                             widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
    
}
  
plot1 <- am.map2
plot2 <- eur.map
legend <- get_legend(eur.map)
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
# 3. Remove the legend from the box plot
eur.map <- eur.map + theme(legend.position="none")
grid.arrange(am.map2, eur.map, legend, ncol=3, widths=c(2.8, 2.8, 0.8))
grid.arrange(am.map2, eur.map, legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))

plot_grid(plot1, eur.map, labels=c("A", "B"), ncol = 2, nrow = 1)

grid_arrange_shared_legend(plot1, plot2, ncol = 2,
                           widths = c(2.8, 2.8), heights = 2.2)

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)

## See if significant
setwd("~/Documents/git/springfreeze")
all<-read.csv("input/all.lat.csv", header=TRUE)

all.lm<-lm(all$Latitude~all$hf.gdd+all$Longitude)
summary(all.lm)


ggplot(all, aes(x=Latitude, y=hf.gdd)) + geom_point() + geom_smooth(method="loess")


