library(diveMove)
library(stringr)
setwd("/Users/GR/Desktop/penguin analysis/signy analysis") 

trip_info <- read.csv("trip_info_signy_may4.csv")
trip_info$year<-substr(trip_info$id,7,11)
info_1516<-subset(trip_info,trip_info$year=="15_16")

pg_ids <-unique(info_1516$id) #these give list of individual penguins that go to sea - each only appear once

setwd("/Users/GR/Desktop/penguin data/signy") 
master<-read.csv(file = "chinstrap deployment.csv")
master<-master[,c("round","bird.number","Stage","Deployment.date","Recovery.date","GPS.file","TDR.file","Edited.TDR.file")]
names(master) <- c("round", "bird","stage","deploy","recover","gps_file","tdr_file","edited_tdr")
data15_16<-master[22:48,] 

start_penguin<-1
end_penguin<-length()
trip_number<-0 #first trip will be this num +1
a<-trip_number
create_df<-data.frame()

for (i in start_penguin:end_penguin){
  setwd("/Users/GR/Desktop/penguin data/signy/original TDR 15_16")
  print(paste("i=",i))
  round_num <-substr(pg_ids[i],13,13)
  bird_num <-substr(pg_ids[i],15,nchar(as.character(pg_ids[i])))

  extension<-data15_16$tdr_file[data15_16$round == round_num & data15_16$bird == bird_num]
  tdr_name<-substr(extension,14,36)
  
  one_tdr <- read.csv(tdr_name,skip=6)

  #-------------fixing time formatting  STARTS--------------------------

  one_tdr<-subset(one_tdr,!is.na(one_tdr$Pressure))
  
  complete_dates<-subset(one_tdr,nchar(as.character(one_tdr$Date.Time.Stamp))==19)
  complete_dates$format_ready<-paste(substr(complete_dates$Date.Time.Stamp,1,6),substr(complete_dates$Date.Time.Stamp,9,19),sep="")

  incomplete_dates<-subset(one_tdr,nchar(as.character(one_tdr$Date.Time.Stamp))!=19)

  #now doing this for data with incomplete dates - because complete and incomplete dates may be different 
  first_minute<-subset(incomplete_dates,incomplete_dates$Date.Time.Stamp==incomplete_dates$Date.Time.Stamp[1])
  if (length(first_minute[,1])==60){
    secs<-rep(c(0:59),100000)[1:length(incomplete_dates[,1])]
  }else{ 
    secs<-rep(seq(0,58,2),100000)[1:length(incomplete_dates[,1])]
  }
  
  if (nrow(incomplete_dates)!=0){ #if there are incomplete dates 
    secs_2<-str_pad(secs, 2, pad = "0") #adding seconds to end of time
    incomplete_dates$time_w_sec<-paste(as.character(incomplete_dates$Date.Time.Stamp),":",secs_2,sep="")
    incomplete_dates$just_date<-gsub( " .*$", "", incomplete_dates$time_w_sec ) #takes everything before the space - date
    incomplete_dates$just_time<-gsub('.* ', '', incomplete_dates$time_w_sec)
    incomplete_dates$right_time<-str_pad(incomplete_dates$just_time, 8, pad = "0")
    incomplete_dates$format_ready<-paste(incomplete_dates$just_date,incomplete_dates$right_time,sep=" ")
    
    if (nrow(complete_dates)==0){
      all_clean<-incomplete_dates[,c("format_ready","Pressure","Temp")]
    }else{
      if (incomplete_dates$format_ready[1]< complete_dates$format_ready[1]){
        all_clean<-rbind(incomplete_dates[,c("format_ready","Pressure","Temp")],complete_dates[,c("format_ready","Pressure","Temp")])
      }else{
        all_clean<-rbind(complete_dates[,c("format_ready","Pressure","Temp")],incomplete_dates[,c("format_ready","Pressure","Temp")])
      }
    }
  } else{# no incomplete dates 
    all_clean<-complete_dates[,c("format_ready","Pressure","Temp")]
  }
  
  #some time has seconds and some doesn't,
  all_clean$POSIXct<-as.POSIXct(all_clean$format_ready,format = "%d/%m/%y %H:%M:%S", tz = "gmt")
  
  new_ordered<- all_clean[order(as.Date(all_clean$POSIXct)),] #order by time stamp
  new_ordered$POSIXct<-as.POSIXct(new_ordered$format_ready,format = "%d/%m/%y %H:%M:%S", tz = "gmt") #

  #write csv file with fixed times, as readTDR must read a csv file
  setwd("/Users/GR/Desktop/penguin data/signy/Jessy fixed dates original TDR 15_16") 
  write.csv(file = tdr_name, new_ordered)  
  
  #-------------fixing time formatting ENDS--------------------------
  
  read_tdr<-readTDR(tdr_name, skip = 0,dateCol=2, depthCol=3,concurrentCols=4, 
                    speed=FALSE,dtformat = "%d/%m/%y %H:%M:%S", tz = "GMT")
  one_tdr<-new_ordered

  id_name<-paste("Signy_15_16",round_num,bird_num,sep="_")
  one_tdr$id <-id_name
  one_tdr$Rec..<-c(1:length(one_tdr$id))
  locs<-which(trip_info$id %in% one_tdr$id) #gives locations of this id in the list 
  
  for(k in 1:length(locs)){
    print("again")
    a=a+1 #this is the trip number, 
    print(paste("a=",a))
    start <- as.POSIXct(trip_info$leave_land[locs[k]])
    finish <- as.POSIXct(trip_info$backon_land[locs[k]])
    
    all_secs<-substr(one_tdr$format_ready,nchar(one_tdr$format_ready)-1,nchar(one_tdr$format_ready))
    if ("59" %in% all_secs){
      variable<-5
    }else{
      variable<-2
    }

    #start time
    start_time<-floor((one_tdr$Rec..[one_tdr$POSIXct==start])/variable)+1 
    if (length(start_time)==0){ 
      start_time<-floor((one_tdr$Rec..[one_tdr$POSIXct==start-1])/variable)+1 
    }
    if(length(start_time)!=0) {#if the TDR ends after the GPS at sea time STARTS can use- exclude this trip as we have no TDR data
      #end time
      end_time<-ceiling((one_tdr$Rec..[one_tdr$POSIXct==finish])/variable)+1
      if(length(end_time)==0) {#if the TDR ends before the GPS at sea time ends, use last TDR time 
        end_time<-ceiling((max(one_tdr$Rec..))/variable) 
      }

     test <-read_tdr[start_time:end_time]
      K <- c(10, 50)# specify the smoothing and filtering windows
      P <- c(0.5, 0.05)
      
      dcalib<- calibrateDepth(test, dive.thr=2,zoc.method="filter",k = K, probs= P, 
                           depth.bounds =c(-2,2),descent.crit.q=0.01, ascent.crit.q=0,knot.factor=20)

      plot(dcalib@tdr@time, -dcalib@tdr@depth, type = "l",main=paste(id_name,"trip",a,sep=" "))
    
      zoc_sea_tdr<-cbind(dcalib@tdr@time,dcalib@tdr@depth,dcalib@tdr@concurrentData)
    
      colnames(zoc_sea_tdr) <- c("datetime","depth","temp")
      zoc_sea_tdr$trip_num <-a # total trip number 
      zoc_sea_tdr$id <-paste("Signy_15_16",round_num,bird_num,sep="_")
    }
    create_df<-rbind(create_df,zoc_sea_tdr)
  }
}  

setwd("/Users/GR/Desktop/signy ZOC TDR") 
write.csv(file = "Signy_15_16_tdr_zoc_at_sea_August25.csv", create_df)  
