######################################################################################################
################################### SUPPLEMENTARY! ###################################################
######################################################################################################
ggplot((df), aes( x=bb.jd, y=risk)) + geom_smooth(method="lm", se=FALSE) + geom_point(aes(col=si))

lmodel<-lm(risk~bb.jd + year,data=df)
display(lmodel)

# Summarize Data
early<- df%>%
  filter(si=="early")
summary(early$risk)
early$sd<-sd(early$risk)
early$mean<-mean(early$risk)
early.lm<-lm(early$bb.jd~early$risk)
summary(early.lm)
plot(early$bb.jd, early$risk)
late<- df%>%
  filter(si=="late")
late.lm<-lm(late$risk~late$bb.jd)
summary(late.lm)
plot(late$bb.jd,late$risk)
summary(late$risk)
late$sd<-sd(late$risk)
late$mean<-mean(late$risk)

# Integrate weather data and risk data

risk.climate<- df %>%
  dplyr::select(year, bb.jd, l75.jd, risk, species) %>%
  gather(phenophase, JD, -year, -risk, -species) %>%
  filter(year==2010) 
risk.climate$phenophase[risk.climate$phenophase == "bb.jd"] <- 1
risk.climate$phenophase[risk.climate$phenophase == "l75.jd"] <- 2
risk.climate<-risk.climate %>%
  unite(species.phase, species, phenophase, remove=TRUE)

risk14<- df %>%
  dplyr::select(year, bb.jd, l75.jd, risk, species) %>%
  gather(phenophase, JD, -year, -risk, -species) %>%
  filter(year==2014) 
risk14$phenophase[risk14$phenophase == "bb.jd"] <- 1
risk14$phenophase[risk14$phenophase == "l75.jd"] <- 2
risk14<-risk14 %>%
  unite(species.phase, species, phenophase, remove=TRUE)

ten<-full_join(w, risk.climate, by="JD") %>%
  dplyr::select(JD,AirT,year,risk,species.phase) 

four<-full_join(w14, risk14, by="JD") %>%
  dplyr::select(JD,AirT,year,risk,species.phase) 

# 1 create vector with unique names of species
# 2 create array/dataframe with nrows=numbsps and 2columns
# 3 generate a for loop for (i in 1:numbsps) that subsets data for target species and stores mean
# and range computed from the two values for that species
#ten<- data.frame(JD=rnorm(42), AirT=rnorm(42),year=rep(2010,42), species=c(rep(c(a_1...))))
#for (j in 2008:2010){}
ten$only.sps<-substr(ten$species.phase,1,4)
ten$only.phase<-substr(ten$species.phase, 6,6)

unique.sps<-as.character(unique(ten$only.sps))
#unique.sps<-unique.sps[-is.na(unique.sps)]
unique.sps<-unique.sps[-3]

store.results<-array(NA,dim=c(length(unique.sps),2))
colnames(store.results)<-c("mean","sd")
i=1
for(i in 1:nrow(store.results)){
  print(i)
  spsi<-unique.sps[i]
  position.1<- which(ten$only.sps==spsi)[1]
  position.2<- which(ten$only.sps==spsi)[2]
  
  temp.vector<-ten$AirT[position.1:position.2]
  #Air.temp1<-subset(ten,only.sps==spsi & only.phase==1)[1,"AirT"]
  #Air.temp2<-subset(ten,only.sps==spsi & only.phase==2)[1,"AirT"]
  
  store.results[i,1]<-mean(temp.vector)
  #store.results[i,2]<-range(temp.vector)[2]-range(temp.vector)[1]
  store.results[i,2]<-sd(temp.vector)
}
#### Year 2014
four$only.sps<-substr(four$species.phase,1,4)
four$only.phase<-substr(four$species.phase, 6,6)

unique.sps<-as.character(unique(four$only.sps))
#unique.sps<-unique.sps[-is.na(unique.sps)]
unique.sps<-unique.sps[-8]

store.results.14<-array(NA,dim=c(length(unique.sps),2))
colnames(store.results.14)<-c("mean","sd")
i=1
for(i in 1:nrow(store.results)){
  print(i)
  spsi<-unique.sps[i]
  position.1<- which(four$only.sps==spsi)[1]
  position.2<- which(four$only.sps==spsi)[2]
  
  temp.vector<-four$AirT[position.1:position.2]
  #Air.temp1<-subset(ten,only.sps==spsi & only.phase==1)[1,"AirT"]
  #Air.temp2<-subset(ten,only.sps==spsi & only.phase==2)[1,"AirT"]
  
  store.results.14[i,1]<-mean(temp.vector)
  #store.results[i,2]<-range(temp.vector)[2]-range(temp.vector)[1]
  store.results.14[i,2]<-sd(temp.vector)
}

# Make Means vs. Risk plots for each species
ten.results<-as.data.frame(store.results)
risk10<-risk.climate %>%
  dplyr::select(species.phase, risk)
risk10$species<-substr(risk10$species.phase, 1,4)
risk10$phase<-substr(risk10$species.phase,6,6)
risk10$phase<-ifelse(risk10$phase==2, NA, 1)
risk10<-na.omit(risk10)
risk10 <- merge(ten.results, risk10, by=0, all=TRUE)  # merge by row names (by=0 or by="row.names")
risk10<- risk10 %>%
  dplyr::select(species, risk, mean, sd)
ten.plot<- ggplot((risk10), aes(x=mean, risk)) + geom_point(aes(col=species))
ten.plot

four.results<-as.data.frame(store.results.14)
risk14<-risk14 %>%
  dplyr::select(species.phase, risk)
risk14$species<-substr(risk14$species.phase, 1,4)
risk14$phase<-substr(risk14$species.phase,6,6)
risk14$phase<-ifelse(risk14$phase==2, NA, 1)
risk14<-na.omit(risk14)
risk14 <- merge(four.results, risk14, by=0, all=TRUE)  # merge by row names (by=0 or by="row.names")              
risk14<- risk14 %>%
  dplyr::select(species, risk, mean, sd)
four.plot<- ggplot((risk14), aes(x=mean, risk)) + geom_point(aes(col=species))
four.plot

ten.mean<- ten %>%
  summarise_each(funs(mean,sd), AirT)

four.mean<- four %>%
  summarise_each(funs(mean,sd), AirT)


mod<-lm(risk~AirT, data = risk.climate.df)
summary(mod)

## Attempt to make overlapping graph
risk.climate.df<-full_join(risk.climate, df)
risk.plot<- ggplot((risk.climate.df), aes(x=JD, y=species), stat="identity") + geom_point(aes(x= df$bb.jd)) + 
  geom_segment(aes(y = sp.year, yend = sp.year, x = bb.jd, xend = l75.jd, col=si)) +
  geom_point(aes(x=l75.jd, col=si)) + geom_point(aes(col=si)) +
  xlab("Budburst to Leaf Out") +
  ylab("Species")
weather.plot<- ggplot((risk.climate.df), aes(x=JD, AirT))

