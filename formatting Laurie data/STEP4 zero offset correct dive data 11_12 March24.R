library(diveMove)
library(foreach)
setwd("C:/Users/Jessica/Desktop/penguin analysis/11_12 analysis") 
registerDoSEQ()

trip_info <- read.csv("trip_info_11_12_Feb26.csv")
pg_ids <-unique(trip_info$id) #these give list of individual penguins that go to sea - each only appear once

start_penguin<-1
end_penguin<-length(pg_ids)
trip_number<-0 #first trip will be this num +1
a<-trip_number

create_df<-data.frame()

for (i in start_penguin:end_penguin){
  print(paste("i=",i))
  round_num <-substr(pg_ids[i],7,7)
  if(substr(pg_ids[i],9,9)=="0"){
    bird_num <-substr(pg_ids[i],10,10)
  }else{
    bird_num <-substr(pg_ids[i],9,10)
  }
  round_wd<-paste("C:/Users/Jessica/Desktop/penguin data/2011-2012/Laurie Island/Tracking data/Round ",round_num,"/",sep="")
  
  bird_in_round<- c()
  attempt <- 1
  while( length(bird_in_round)==0 && attempt <= 3 ) {
    attempt <- attempt + 1
    try(bird_in_round<-list.files(round_wd,pattern=paste("Bird #",bird_num,sep="")))
  } 
  
  setwd(paste(round_wd,bird_in_round,"/",sep=""))
  tdr_name<-list.files(pattern="*00.csv")
  
  one_tdr<- NULL
  attempt <- 1
  while( is.null(one_tdr) && attempt <= 3 ) {
    attempt <- attempt + 1
    try(one_tdr <- read.csv(tdr_name,skip=2))
  } 
  
  first_thing<-gsub("/.*$", "", unique(one_tdr$Date))
  
  if(colnames(one_tdr)[4]=="Pressure..dBars."){ 
    if (length(first_thing)==length(unique(first_thing))){ 
      one_tdr$POSIXct <- as.POSIXct(paste(one_tdr$Date, one_tdr$Time, sep = " "),
                                    format = "%d/%m/%Y %H:%M:%S", tz = "gmt")
      read_tdr<-readTDR(tdr_name, skip = 2,dateCol=2,timeCol=3, depthCol=4, # 
                        speed=FALSE,concurrentCols = 5:6,dtformat = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    }else{
      print("hi")
      one_tdr$POSIXct <- as.POSIXct(paste(one_tdr$Date, one_tdr$Time, sep = " "),
                                    format = "%m/%d/%Y %H:%M:%S", tz = "gmt")
      read_tdr<-readTDR(tdr_name, skip = 2,dateCol=2,timeCol=3, depthCol=4,
                        speed=FALSE,concurrentCols = 5:6,dtformat = "%m/%d/%Y %H:%M:%S", tz = "GMT")
    }
  }else if (colnames(one_tdr)[6]=="Pressure..dBars."){ 
    if (length(first_thing)==length(unique(first_thing))){ 
      one_tdr$POSIXct <- as.POSIXct(paste(one_tdr$Date, one_tdr$Time, sep = " "),
                                    format = "%d/%m/%Y %H:%M:%S", tz = "gmt")
      read_tdr<-readTDR(tdr_name, skip = 2,dateCol=2,timeCol=3, depthCol=6, 
                        speed=FALSE,concurrentCols = 4:5,dtformat = "%d/%m/%Y %H:%M:%S", tz = "GMT")
    }else{
      print("hi")
      one_tdr$POSIXct <- as.POSIXct(paste(one_tdr$Date, one_tdr$Time, sep = " "),
                                    format = "%m/%d/%Y %H:%M:%S", tz = "gmt")
      read_tdr<-readTDR(tdr_name, skip = 2,dateCol=2,timeCol=3, depthCol=6,
                        speed=FALSE,concurrentCols = 4:5,dtformat = "%m/%d/%Y %H:%M:%S", tz = "GMT")
    }
  }
  
  if(substr(bird_num,2,2)==""){#single digit number
    id_name<-paste("11_12",round_num,paste("0",bird_num,sep=""),sep="_")
  }else{
    id_name<-paste("11_12",round_num,bird_num,sep="_")
  }
  one_tdr$id <-id_name
  
  locs<-which(trip_info$id %in% one_tdr$id) #gives locations of this id in the list 
  
  for(k in 1:length(locs)){
    print("again")
    a=a+1 
    print(paste("a=",a))
    start <- as.POSIXct(trip_info$leave_land[locs[k]])
    finish <- as.POSIXct(trip_info$backon_land[locs[k]])
    
    #start time 
    start_time<-floor((one_tdr$Rec..[one_tdr$POSIXct==start])/5)+1 

    if(length(start_time)!=0) {#if the TDR ends after the GPS at sea time STARTS can use- exclude this trip as we have no TDR data
      #end time
      end_time<-ceiling((one_tdr$Rec..[one_tdr$POSIXct==finish])/5)+1
      if(length(end_time)==0) {#if the TDR ends before the GPS at sea time ends, use last TDR time 
        end_time<-ceiling((max(one_tdr$Rec..))/5) 
      }

     test <-read_tdr[start_time:end_time]
      K <- c(10, 50)# specify the smoothing and filtering windows
      P <- c(0.5, 0.05)# 
      dcalib<- calibrateDepth(test, dive.thr=2,zoc.method="filter",k = K, probs= P, 
                           depth.bounds =c(-2,2),descent.crit.q=0.01, ascent.crit.q=0,knot.factor=20)
      
      
      diveinfo<-diveStats(dcalib, depth.deriv=TRUE)
      diveinfo$trip_num <-a # total trip number 
      diveinfo$id <-paste("11_12",round_num,bird_num,sep="_")
    }
    create_df<-rbind(create_df,diveinfo)
  }
}  

head(create_df)
tail(create_df)

setwd("C:/Users/Jessica/Desktop/penguin analysis/11_12 analysis") 
write.csv(file = "11_12_diveinfo_Mar24.csv", create_df)  
