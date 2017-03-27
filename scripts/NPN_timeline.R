## Attempting to find more compelling data from NPN
# Cat - 23 March 2017
## After working with Tree Spotters data, I have realized it isn't informative enough
# Observer bias is skewing data results so we need a bigger dataset
# hoping to integrate John O'Keefes data from 2010 and 2014 and see if 
# we can find comparisons or more interesting results

# Clear workspace
rm(list=ls()) # remove everything currently held in the R memory
options(stringsAsFactors=FALSE)
graphics.off()

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(arm)
library(data.table)
# Set Working Directory
setwd("~/Documents/git/springfreeze")
sixteen<-read.csv("input/NPN_2016.csv",header=TRUE)
fourteen<-read.csv("input/NPN_2014.csv",header=TRUE)
ten<-read.csv("input/NPN_2010.csv",header=TRUE)

phases<-c("Budburst","Leaves")

pheno16<-sixteen%>%
  dplyr::select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude, NumYs_in_Series) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  filter(Latitude>35 & Latitude<50)%>%
  rename(Year = First_Yes_Year) %>%
  filter(NumYs_in_Series>1)

df<-pheno16%>%
  dplyr::select(-NumYs_in_Series)%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
df<-na.omit(df)
df$Risk <- df$Leaves - df$Budburst
df<-filter(df, Risk > 0)
df<-ungroup(df)

df$mean<-ave(df$Budburst, df$species)
df$stand_dev<-ave(df$Budburst, df$species, FUN=sd)
df$mean.leaf<-ave(df$Leaves, df$species)
df$stand_dev.leaf<-ave(df$Leaves, df$species, FUN=sd)

y<- df %>%
  dplyr::select(species, Budburst, Leaves, Risk, mean, stand_dev, mean.leaf, stand_dev.leaf) %>%
  gather(Phenophase, DOY, -species, -Risk, -mean, -stand_dev, -mean.leaf, -stand_dev.leaf)
y<-y %>%
  group_by(species, mean, Phenophase)%>%
  arrange(species)%>%
  filter(row_number()==1)%>%
  ungroup(y)


bud<- df %>%
  dplyr::select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves<- df %>%
  dplyr::select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)

stand_dev<- y %>%
  dplyr::select(species, stand_dev, stand_dev.leaf)

basic<- full_join(bud, leaves)
basic$Risk<- basic$Leaves - basic$Budburst
basic<-full_join(basic, stand_dev)
basic$code <- reorder(basic$species, basic$Budburst)
basic<-na.omit(basic)
ts.timeline<-ggplot((basic), aes(x=Budburst, y=code), stat="identity") + geom_point(aes(x= basic$Budburst)) + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic$Leaves)) + theme(legend.position="none") + geom_point() + xlab("Budburst to Leaf Out") +
  ylab("Species") +geom_errorbarh(aes(xmin=Budburst-stand_dev, xmax=Budburst+stand_dev, col="coral"), height=.0) + 
  geom_errorbarh(aes(xmin=Leaves-stand_dev.leaf, xmax=Leaves+stand_dev.leaf, col="forestgreen"), height=.0)
plot(ts.timeline)

y$code <- reorder(y$species, y$DOY)
y$stand_dev<-ifelse(y$stand_dev==0, NA, y$stand_dev)
y<-na.omit(y)

ts<-ggplot((y), aes(x=code, y=DOY)) + geom_point(aes(col=Phenophase)) + 
  xlab("Day of Year") + ylab("Species") + geom_errorbar(aes(ymin=DOY-stand_dev, ymax=DOY+stand_dev, col=Phenophase), width=.0) +
  scale_x_discrete(labels = abbreviate)
  #scale_x_discrete(labels = c("POPDEL","FRAAME", "MALSPP", "PRUSER","ACERUB", "BETALL", "BETPAP", "FAGGRA",
                              #"QUERUB", "BETNIG", "QUEALB", "ULMAME", "ACESAC", "POPTRE"))
plot(ts)

# Analysis 2016
hist(coef(lm16)$species[,1],)

lm16<-lmer(Risk~Budburst + Latitude + (1|species), data=df)
display(lm16)

coef(lm16)$species[,1]
ranef(lm16)$species[,1]
##### 2014 ##########
pheno14<-fourteen%>%
  dplyr::select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude, NumYs_in_Series) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) %>%
  filter(NumYs_in_Series>1)

df14<-pheno14%>%
  dplyr::select(-NumYs_in_Series) %>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
df14<-na.omit(df14)
df14$Risk <- df14$Leaves - df14$Budburst
df14<-filter(df14, Risk > 0)
df14<-ungroup(df14)

df14$mean<-ave(df14$Budburst, df14$species)
df14$stand_dev<-ave(df14$Budburst, df14$species, FUN=sd)
df14$mean.leaf<-ave(df14$Leaves, df14$species)
df14$stand_dev.leaf<-ave(df14$Leaves, df14$species, FUN=sd)

y14<- df14 %>%
  dplyr::select(species, Budburst, Leaves, Risk, mean, stand_dev, mean.leaf, stand_dev.leaf) %>%
  gather(Phenophase, DOY, -species, -Risk, -mean, -stand_dev, -mean.leaf, -stand_dev.leaf)
y14<-y14 %>%
  group_by(species, mean, Phenophase)%>%
  arrange(species)%>%
  filter(row_number()==1)%>%
  ungroup(y14)


bud14<- df14 %>%
  dplyr::select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves14<- df14 %>%
  dplyr::select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)
stand_dev14<- df14 %>%
  dplyr::select(species, stand_dev, stand_dev.leaf)


basic14<- full_join(bud14, leaves14)
basic14<-full_join(basic14, stand_dev14)
basic14$Risk<- basic14$Leaves - basic14$Budburst
basic14$code <- reorder(basic14$species, basic14$Budburst)
basic14<-na.omit(basic14)
ts.timeline14<-ggplot((basic14), aes(x=Budburst, y=code), stat="identity") + geom_point() + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic14$Leaves)) + theme(legend.position="none") + geom_point() + xlab("Budburst to Leaf Out") +
  ylab("Species") + geom_errorbarh(aes(xmin=Budburst-stand_dev, xmax=Budburst+stand_dev, col="coral"), height=.0) + 
  geom_errorbarh(aes(xmin=Leaves-stand_dev.leaf, xmax=Leaves+stand_dev.leaf, col="forestgreen"), height=.0)
plot(ts.timeline14)

y14$code <- reorder(y14$species, y14$DOY)

ts14<-ggplot((y14), aes(x=code, y=DOY)) + geom_point(aes(col=Phenophase)) + 
  xlab("Day of Year") + ylab("Species") + geom_errorbar(aes(ymin=DOY-stand_dev, ymax=DOY+stand_dev, col=Phenophase), width=.0)
plot(ts14)

lm14<-lmer(Risk~Budburst + Latitude + (1|species), data=df14)
display(lm14)


########## 2010 ##############
pheno10<-ten%>%
  dplyr::select(Site_ID, Genus, Species, Individual_ID, Phenophase_Description, First_Yes_DOY, First_Yes_Year, Latitude, Longitude, NumYs_in_Series) %>%
  unite(species, Genus, Species, sep="_") %>%
  filter(Phenophase_Description %in% phases) %>%
  rename(Year = First_Yes_Year) %>%
  filter(NumYs_in_Series>1)

df10<-pheno10%>%
  dplyr::select(-NumYs_in_Series)%>%
  group_by(species, Individual_ID, Phenophase_Description)%>%
  arrange(Individual_ID) %>%
  filter(row_number()==1) %>%
  spread(Phenophase_Description, First_Yes_DOY)
df10<-na.omit(df10)
df10$Risk <- df10$Leaves - df10$Budburst
df10<-filter(df10, Risk > 0)
df10<-ungroup(df10)

df10$mean<-ave(df10$Budburst, df10$species)
df10$stand_dev<-ave(df10$Budburst, df10$species, FUN=sd)
df10$mean.leaf<-ave(df10$Leaves, df10$species)
df10$stand_dev.leaf<-ave(df10$Leaves, df10$species, FUN=sd)

y10<- df10 %>%
  dplyr::select(species, Budburst, Leaves, Risk, mean, stand_dev, mean.leaf, stand_dev.leaf) %>%
  gather(Phenophase, DOY, -species, -Risk, -mean, -stand_dev, -mean.leaf, -stand_dev.leaf)
y10<-y10 %>%
  group_by(species, mean, Phenophase)%>%
  arrange(species)%>%
  filter(row_number()==1)%>%
  ungroup(y10)


bud10<- df10 %>%
  dplyr::select(species, Budburst) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Budburst) %>%
  arrange(species)

leaves10<- df10 %>%
  dplyr::select(species, Leaves) %>%
  group_by(species)%>%
  summarise_each(funs(mean), Leaves) %>%
  arrange(species)

stand_dev10<- y10 %>%
  dplyr::select(species, stand_dev, stand_dev.leaf)

basic10<- full_join(bud10, leaves10)
basic10$Risk<- basic10$Leaves - basic10$Budburst
basic10<-full_join(basic10, stand_dev10)
basic10$code <- reorder(basic10$species, basic10$Budburst)
basic10<-na.omit(basic10)
ts.timeline<-ggplot((basic10), aes(x=Budburst, y=code), stat="identity") + geom_point(aes(x= basic10$Budburst)) + 
  geom_segment(aes(y = species, yend = species, x = Budburst, xend = Leaves)) + 
  geom_point(aes(x=basic10$Leaves)) + theme(legend.position="none") + geom_point() + xlab("Budburst to Leaf Out") +
  ylab("Species") +geom_errorbarh(aes(xmin=Budburst-stand_dev, xmax=Budburst+stand_dev, col="coral"), height=.0) + 
  geom_errorbarh(aes(xmin=Leaves-stand_dev.leaf, xmax=Leaves+stand_dev.leaf, col="forestgreen"), height=.0)
plot(ts.timeline)

y10$code <- reorder(y10$species, y10$DOY)

ts<-ggplot((y10), aes(x=code, y=DOY)) + geom_point(aes(col=Phenophase)) + 
  xlab("Day of Year") + ylab("Species") + geom_errorbar(aes(ymin=DOY-stand_dev, ymax=DOY+stand_dev, col=Phenophase), width=.0) #+ 
  #scale_x_discrete(labels = c("POPDEL","FRAAME", "MALSPP", "PRUSER","ACERUB", "BETALL", "BETPAP", "FAGGRA",
                              #"QUERUB", "BETNIG", "QUEALB", "ULMAME", "ACESAC", "POPTRE"))
plot(ts)

lm10<-lmer(Risk~Budburst + Latitude + (1|species), data=df10)
display(lm10)
