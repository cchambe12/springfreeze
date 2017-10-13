## Stan model with experiment data and duration of vegetative risk
# Based on script by Dan Flynn and Lizzie from Buds repo 

# Basic housekeeping
rm(list=ls()) 
options(stringsAsFactors=FALSE)

# Fake data for buburst stan work #
library(dplyr)

setwd("~/Documents/git/springfreeze/scripts/stan")

# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>
# Set up: same as experiment, with two sites, 28 species, two levels each of warming and photoperiod, and three levels of chilling. 2016-04-01 adding interactions. This ends up generating expected differences, but variation in effect sizes across species is minimal currently.
# <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <> <>

nsp = 9

nsite = 2
nwarm = 2
nphoto = 2
nchill = 3 ### should this be 3? Am I including all treatments or just the two?

rep = 6 # within each combination of treatments. 

(ntot = nsite*nwarm*nphoto*nchill*rep) # 792 rows; 22k rows across species

# Build up the data frame
site = gl(nsite, rep, length=ntot)
warm = gl(nwarm, rep*nsite, length = ntot)
photo = gl(nphoto, rep*nwarm*nsite, length = ntot)
chill = gl(nchill, rep*nwarm*nphoto*nsite, length = ntot)

chill1 = ifelse(chill == 2, 1, 0) 
chill2 = ifelse(chill == 3, 1, 0) 

treatcombo = paste(warm, photo, chill1, chill2, sep = "_")

(d <- data.frame(site, warm, photo, chill1, chill2, treatcombo)) # critical coding error here!

###### Set up differences for each level
sitediff = 2
warmdiff = -10 # days earlier from 1 to 2
photodiff = -10
chill1diff = -5
chill2diff = -4

# interactions. 9 two-way interactions
sitewarm = 0
sitephoto = 0
sitechill1 = 0
sitechill2 = 0
warmphoto = 5 # positive 3.5. So at the warm level, the effect of longer days is muted by 3.5 days.
warmchill1 = 3 # both positive ~ 10. 
warmchill2 = 3
photochill1 = 2 # from stan results
photochill2 = 2

######## SD for each treatment
sitediff.sd = 1
warmdiff.sd = 1 
photodiff.sd = 1
chill1diff.sd = 1
chill2diff.sd = 1

# interactions. 9 two-way interactions
sitewarm.sd = 1
sitephoto.sd = 1
sitechill1.sd = 1 
sitechill2.sd = 1
warmphoto.sd = 1
warmchill1.sd = 1
warmchill2.sd = 1
photochill1.sd = 1
photochill2.sd = 1


mm <- model.matrix(~(site+warm+photo+chill1+chill2)^2, data.frame(warm, photo))
# remove last column, chill1 x chill2, empty
mm <- mm[,-grep("chill1:chill2", colnames(mm))]
colnames(mm)

coeff <- c(1, sitediff, warmdiff, photodiff, chill1diff, chill2diff,
           sitewarm, sitephoto, sitechill1, sitechill2,
           warmphoto, warmchill1, warmchill2,
           photochill1, photochill2
)


risk <- rnorm(n = length(warm), mean = mm %*% coeff, sd = 1) # should be able to do sd = mm %*% sd.coeff as well, with a different sd for each parameter.

(fake <- data_frame(risk, site, warm, photo, chill1, chill2))

summary(lm(risk ~ (site+warm+photo+chill1+chill2)^2, data = fake)) # sanity check 

##### Again, now with species now.

baseinter = 20 # baseline intercept across all species for DVR
spint <- baseinter + c(1:nsp)-mean(1:nsp) # different intercepts by species

fake <- vector()

for(i in 1:nsp){ # loop over species, as these are the random effect modeled
  
  # Give species different difference values, drawn from normal. Could make dataframe of diffs and diff.sds, and use apply..
  
  coeff <- c(spint[i], 
             rnorm(1, sitediff, sitediff.sd),
             rnorm(1, warmdiff, warmdiff.sd),
             rnorm(1, photodiff, photodiff.sd), 
             rnorm(1, chill1diff, chill1diff.sd),
             rnorm(1, chill2diff, chill2diff.sd), 
             rnorm(1, sitewarm, sitewarm.sd), 
             rnorm(1, sitephoto, sitephoto.sd),
             rnorm(1, sitechill1, sitechill1.sd),
             rnorm(1, sitechill2, sitechill2.sd),
             rnorm(1, warmphoto, warmphoto.sd),
             rnorm(1, warmchill1, warmchill1.sd),
             rnorm(1, warmchill2, warmchill2.sd),
             rnorm(1, photochill1, photochill1.sd),
             rnorm(1, photochill2, photochill2.sd)
  )
  
  risk <- rnorm(n = length(warm), mean = mm %*% coeff, sd = 0.1)
  
  fakex <- data.frame(risk, sp = i, site, warm, photo, chill1, chill2)
      
  fake <- rbind(fake, fakex)  
  }

summary(lm(risk ~ (site+warm+photo+chill1+chill2)^2, data = fake)) # sanity check 

# now fix the levels to 0/1 (not 1/2) as R does
fake$site <- as.numeric(fake$site)
fake$site[fake$site==1] <- 0
fake$site[fake$site==2] <- 1

fake$warm <- as.numeric(fake$warm)
fake$warm[fake$warm==1] <- 0
fake$warm[fake$warm==2] <- 1

fake$photo <- as.numeric(fake$photo)
fake$photo[fake$photo==1] <- 0
fake$photo[fake$photo==2] <- 1

summary(lm(risk ~ (site+warm+photo+chill1+chill2)^2, data = fake)) # double sanity check 

#summary(lmer(bb ~ (site|sp) + (warm|sp) + (photo|sp) + (chill1|sp) + (chill2|sp), data = fake)) # too hard for lmer.

save(list=c("fake"), file = "Fake Budburst.RData")
#write.csv(fake, file="~/Documents/git/springfreeze/output/fakedata_exp.csv", row.names = FALSE)


