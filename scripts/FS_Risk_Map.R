## 14 March 2017 - Cat
# Attempt to work on gridded daily climate layers for false spring project

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(lattice)
library(chron)
library(ncdf4)
library(RColorBrewer)

# Set Working Directory
setwd("~/Documents/git/springfreeze/input")
nc<-nc_open("griddedClimate")
print(nc)

# Get longitude, latitude and time data
long<-ncvar_get(nc,"longitude")
nlong<-dim(long)
lat<-ncvar_get(nc,"latitude")
nlat<-dim(lat)
print(c(nlong,nlat))

doy<-ncvar_get(nc,"day_of_year")
year<-ncvar_get(nc,"year")

# Get temperature data
tmean<-ncvar_get(nc, "temperature")
name<-ncatt_get(nc, "temperature", "long_name")
units<-ncatt_get(nc, "temperature", "units")
fillvalue<-ncatt_get(nc, "temperature", "_Fillvalue")
gdd<-tmean-5
frz<-ifelse(tmean<=-2.2, TRUE, FALSE)
risk<-ifelse(doy>=60 & doy<=210, TRUE, FALSE)
count<- ave(
  gdd, year, 
  FUN=function(x) cumsum(c(0, head(x, -1)))
)
spring<-ifelse(count>=100 & count<=400, TRUE, FALSE)

d<-data.frame(tmean, gdd, spring, frz)

df<-data.frame(doy, year, risk)

tmean[tmean==fillvalue$value]<-NA
length(na.omit(as.vector(tmean[,,1])))

# Make a map...
m<-1
temp.slice<-tmean[,,m]
image(long, lat, temp.slice, col=rev(brewer.pal(10,"RdBu")))

grid <- expand.grid(long = long, lat = lat)
cutpts <- c(-50, -40, -30, -20, -10, 0, 10, 20, 30, 40, 50)
levelplot(temp.slice ~ long * lat, data = grid, at = cutpts, cuts = 11, pretty = TRUE, 
          col.regions = (rev(brewer.pal(10, "RdBu"))))

d<-data.frame(doy, tmean, gdd, year, spring, frz, risk)
