## 31 May 2017 - Cat
## Attempt to Map out site locations for regional risk plor

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
canada<-map("worldHires","Canada", xlim=c(-141,-53), ylim=c(40,85), col="gray90", fill=TRUE)

gg1<- NAmap <- ggplot() + geom_polygon(data = usa, 
                                       aes(x=long, y = lat, group = group), 
                                       fill = "white", 
                                       color="black") +
  geom_polygon(data = states, aes(x=long, y = lat, group = group), 
               fill = "white", color="grey")

# Upload Data
setwd("~/Documents/git/springfreeze")
euro<-read.csv("input/regional_europe.csv", header=TRUE)
amer<-read.csv("input/regional_usa.csv", header=TRUE)

# USA Map
gg1 + 
  geom_point(data = usa, aes(x = long, y = lat), color = "black", size = 3) +
  geom_point(data = usa, aes(x = long, y = lat), size = 3) + 
  geom_point(fill=factor(usa$False.Springs)) + theme(legend.position="none")

am.map <- gg1 + geom_point(data = amer, aes(Longitude, Latitude, color=Site), size=4)

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

eur.map <- eur + geom_point(data = euro, aes(Longitude, Latitude, color=Site), size =4)




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
legend <- get_legend(eur.map)
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()
# 3. Remove the legend from the box plot
eur.map <- eur.map + theme(legend.position="none")
grid.arrange(am.map, eur.map, legend, ncol=3, widths=c(2.8, 2.8, 0.8))
grid.arrange(am.map, eur.map, legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))

plot_grid(plot1, eur.map, labels=c("A", "B"), ncol = 2, nrow = 1)

grid_arrange_shared_legend(plot1, plot2, ncol = 2,
                           widths = c(4, 2.5), heights = 2.2)
