# Working with O'Keefe Harvard Forest Phenology data, for Cat

library(ggplot2)

# Set your own working directory as necessary
setwd("~/Documents/git/buds/analyses/data")

d <- read.csv("hf003-06-mean-spp.csv")
splist <- read.csv("../okeefe/input/specieslist_bylizzie.csv") # change file path as necessary
d$latbi <- splist[match(d$sp, splist$code), "genusspecies"]


# Plot mean leafout date by year. Two ways of plotting

ggplot(d, aes(year, l75.jd, group = sp)) + 
  geom_smooth(method="lm", se = F, aes(color = sp)) +
  geom_point(aes(color = sp)) 

ggplot(d, aes(year, l75.jd)) + 
  geom_smooth(method="lm") +
  geom_point(aes(color = species)) +
  facet_wrap(~latbi, ncol = 5)


# Calculating order of leafout and order of budburst for each year. 

lo <- bb <- sp <- yr <- vector()

for(i in unique(d$year)){ # i = '2000'
  dxx <- d[d$year == i,]
  lox <- rank(dxx$l75.jd)
  lox[is.na(dxx$l75.jd)] = NA
  box <- rank(dxx$bb.jd, na.last = T)
  box[is.na(dxx$bb.jd)] = NA
  lo = c(lo, lox)
  bb = c(bb, box)
  sp = c(sp, dxx$sp)
  yr = c(yr, rep(i, nrow(dxx)))
}

d1 <- data.frame(year = yr, sp, leafout.order = lo, budburst.order = bb, l75 = d$l75.jd)


# plotting order of budburst and order of leafout over years
ggplot(d1, aes(year, budburst.order, group = sp)) + geom_line(aes(color = sp)) 
ggplot(d1, aes(year, leafout.order, group = sp)) + geom_line(aes(color = sp)) 

# checking for the early and later halves of the data set; is order of leafout consistent?
mean.order.1 <- aggregate(leafout.order ~ sp, mean, data = d1[d1$year < 2003,])
se.order.1 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year < 2003,])
mean.order.2 <- aggregate(leafout.order ~ sp, mean, data = d1[d1$year >= 2003,])
se.order.2 <- aggregate(leafout.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year >= 2003,])

bmean.order.1 <- aggregate(budburst.order ~ sp, mean, data = d1[d1$year < 2003,])
bse.order.1 <- aggregate(budburst.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year < 2003,])
bmean.order.2 <- aggregate(budburst.order ~ sp, mean, data = d1[d1$year >= 2003,])
bse.order.2 <- aggregate(budburst.order ~ sp, function(x) sd(x,na.rm=T)/sqrt((length(x[!is.na(x)])-1)), data = d1[d1$year >= 2003,])


# are the orders even internally consistent between these two time periods?
par(mfrow =c(1,2))
xx <- merge(mean.order.1, mean.order.2, by = 'sp', all=T)
bxx <- merge(bmean.order.1, bmean.order.2, by = 'sp', all=T)
with(xx, plot(leafout.order.x, leafout.order.y)) # Yes, not bad!
with(bxx, plot(budburst.order.x, budburst.order.y)) # Quite good actually
