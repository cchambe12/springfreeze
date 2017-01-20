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
library(gridExtra)

# Upload US map
usa <- map_data("usa")
states <- map_data("state")

gg1<- NAmap <- ggplot() + geom_polygon(data = usa, 
                                       aes(x=long, y = lat, group = group), 
                                       fill = "white", 
                                       color="black") +
  geom_polygon(data = states, aes(x=long, y = lat, group = group), 
               fill = "white", color="grey") 

# Upload Data
usa<-read.csv("~/Documents/git/springfreeze/input/america.lat.csv", header=TRUE)
europe<-read.csv("~/Documents/git/springfreeze/input/europe.lat.csv", header=TRUE)

# USA Map
gg1 + 
  geom_point(data = usa, aes(x = Longitude, y = Latitude), color = "black", size = 3) +
  geom_point(data = usa, aes(x = Longitude, y = Latitude), size = 3) + 
  geom_point(fill=factor(usa$False.Springs)) + theme(legend.position="none")

am.map <- gg1 + geom_point(data = usa, aes(Longitude, Latitude, size=False.Springs,color=False.Springs)) +
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

eur.map <- eur + geom_point(data = europe, aes(Longitude, Latitude, size=False.Springs, color=False.Springs)) + 
  scale_color_gradient(low="red", high="blue", name="Number of False Springs") + theme(legend.position="none") + 
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
  
plot1 <- am.map
plot2 <- eur.map

grid_arrange_shared_legend(plot1, plot2, ncol = 2,
                           widths = c(2.5, 2.5), heights = 2.2)

