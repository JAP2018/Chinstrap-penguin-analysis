library(diveMove)
library(foreach)
registerDoSEQ()
setwd("C:/Users/Jessica/Desktop/penguin analysis/15_16 analysis") 
trip_info <- read.csv("trip_info_15_16_Dec11.csv")
pg_ids <-unique(trip_info$id) #these give list of individual penguins that go to sea 

start_penguin<-1
end_penguin<-length(pg_ids)
trip_number<-0
a<-trip_number

create_df<-data.frame()
for (i in start_penguin:end_penguin){
  print(paste("i=",i))
  round_num <-substr(pg_ids[i],7,7)
  bird_num <-substr(pg_ids[i],9,10)
  setwd(paste("C:/Users/Jessica/Desktop/penguin data/2015-2016/Monroe Island/Tracking data/Round "
              ,round_num,"/Bird ",bird_num,"/",sep=""))
  print("s'up")
  
  r <- NULL
  attempt <- 1
  while( is.null(r) && attempt <= 3 ) {
    attempt <- attempt + 1
    try(
      r <- read.csv(list.files(pattern="*00.csv"),skip=2)
    )
  } 
  one_tdr<-r
    print("hi")

  #zero offset
  read_tdr<-readTDR(list.files(pattern="*00.csv"), skip = 2,dateCol=2,timeCol=3, depthCol=6,
                    speed=FALSE,concurrentCols = 4:5,dtformat = "%d/%m/%Y %H:%M:%S", tz = "GMT")
  
  one_tdr$id <-paste("15_16",round_num,bird_num,sep="_") #assign this penguin a number
  locs<-which(trip_info$id %in% one_tdr$id) #gives locations of this id in the list 
  for (k in 1:length(locs)){ 
    a=a+1
    print(paste("a=",a))
    one_tdr$POSIXct <- as.POSIXct(paste(one_tdr$Date, one_tdr$Time, sep = " "),
                                  format = "%d/%m/%Y %H:%M:%S", tz = "gmt")
    start <- as.POSIXct(trip_info$leave_land[locs[k]]) 
    finish <- as.POSIXct(trip_info$backon_land[locs[k]])

    #start time
    start_time<-floor((one_tdr$Rec..[one_tdr$POSIXct==start])/5)+1
    #end time 
    end_time<-ceiling((one_tdr$Rec..[one_tdr$POSIXct==finish])/5)+1
    
    test <-read_tdr[start_time:end_time]
    tt <- getTime(test)
    d <- getDepth(test)
    K <- c(10, 50)# specify the smoothing and filtering windows
    P <- c(0.5, 0.05)
    dcalib<- calibrateDepth(test, dive.thr=2,zoc.method="filter",k = K, probs= P, depth.bounds =c(-2,2),descent.crit.q=0.01, ascent.crit.q=0,knot.factor=20)
    
    diveinfo<- NULL
    try(diveinfo<-diveStats(dcalib, depth.deriv=TRUE))
    if (!is.null(diveinfo)){
      diveinfo$trip_num <-a # total trip number 
      diveinfo$id <-paste("15_16",round_num,bird_num,sep="_")
    }
  }
  if (!is.null(diveinfo)){
    create_df<-rbind(create_df,diveinfo)
  }
}

setwd("C:/Users/Jessica/Desktop/penguin analysis/15_16 analysis")
write.csv(file = "15_16_diveinfo_March24_trip2_67.csv", create_df)  
