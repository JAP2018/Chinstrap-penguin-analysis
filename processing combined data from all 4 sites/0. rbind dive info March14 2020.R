setwd("C:/Users/Jessica/Desktop/Chinstrap analysis Feb8 2020/2 3 year (4 year) data processing Feb8 2020/individual seasons ZOC w new par Feb25, 2019") 
list.files()
diveinfo11_12<- read.csv(list.files()[2])
diveinfo11_12$year<-"11_12"
diveinfo13_14<- read.csv(list.files()[4])
diveinfo13_14$year<-"13_14"
diveinfo15_16<- read.csv(list.files()[6])
diveinfo15_16$year<-"15_16"
diveinfo_signy_15_16<- read.csv(list.files()[8])
diveinfo_signy_15_16$year<-"Signy_15_16"


X.1<-diveinfo15_16$X
diveinfo15_16<-cbind(X.1,diveinfo15_16) #to make it the same as other years so can rbind

allyears_dive<-do.call("rbind", list(diveinfo11_12,diveinfo13_14,diveinfo15_16,diveinfo_signy_15_16))
head(allyears_dive)
tail(allyears_dive)

length(unique(allyears_dive[c("trip_num","id")])[,1]) 

#----------------Calculate time on surface to next dive-------
info_using<- allyears_dive[,c("year","id","trip_num","begdesc","divetim","maxdep","bottdist","botttim","asctim","desctim")]
colnames(info_using) <- c("year","id","trip_num","start_time","duration","max_depth","bott_dist","bott_time","ascent_time","descent_time")
info_using$start_time <- as.POSIXct(info_using$start_time)

sec2next <- c()
for (k in unique(info_using$year)){
  oneyear <- subset(info_using,info_using$year==k)
  for (q in unique(oneyear$trip_num)){
    onetrip <- subset(oneyear,oneyear$trip_num==q)
    for (i in 1:length(onetrip[,1])-1){
      sec2nextdive<-as.numeric(difftime(onetrip$start_time[i+1],onetrip$start_time[i],  units = c("secs")))-onetrip$duration[i]
      sec2next <- append(sec2next,sec2nextdive)
    }
    sec2next <- append(sec2next,NA)#one NA at end of each trip becase no time to next dive 
  }
  
}
info_using$sec2next <- sec2next  

info_using$asc_desc_time <- info_using$ascent_time+info_using$descent_time


write.csv(file = "0. 4year_dives_time2next_Feb24 2020.csv", info_using)

