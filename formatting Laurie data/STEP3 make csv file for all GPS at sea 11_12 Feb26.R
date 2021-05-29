setwd("C:/Users/Jessica/Desktop/penguin analysis/11_12 analysis") 

trip_info <- read.csv("trip_info_11_12_Feb26.csv")
master <- read.csv("GPS_Laurie_Deployment_Data.csv")

setwd("C:/Users/Jessica/Desktop/penguin data/2011-2012/Laurie Island/Tracking data")
a=0
for(i in 1:length(master[,1])){
  #i=1
  call_file <-as.character(master[i,1][[1]])
  one_file <-read.csv(paste(".",substr(call_file,64,nchar(call_file)),".csv",sep=""))
  #removing data points where Lat or Lon ==0 - GPS error
  one_file<-one_file[!rowSums(one_file[9] ==0),]
  one_file<-one_file[!rowSums(one_file[10] ==0),]
  
  if(substr(call_file,80,80)==" "){
    this_id <-paste("11_12",substr(call_file,71,71),paste("0",substr(call_file,79,79),sep=""),sep="_")
  }
  else{
    this_id <-paste("11_12",substr(call_file,71,71),substr(call_file,79,80),sep="_") 
  }
  
  one_file$id <-this_id
  
  if (this_id %in% trip_info$id){ #if the penguin id is contained in file with all trips that go to sea
    #add POSIXct time to the individual gps file --> so can cut it by time in next step
    #it is gmt time
    
    if ((one_file$YY[1]!=11 & one_file$YY[1]!=12) & (one_file$DD[1]==11 | one_file$DD[1]==12)){
      #if the YY columns and DD columns are mixed up, change order of column than make names same as others
      one_file<- one_file[,c("DD","MM","YY","HH","MI","SS","DATE","TIME","XXX","SVC","LAT","LON",
                             "HEIGHT","OFFSET","GOOD","VOLTAGE","id" )]
      colnames(one_file)[1:3] <- c("YY","MM","DD")
    }
    one_file$POSIXct <- as.POSIXct(paste(paste(one_file$DD,one_file$MM,one_file$YY,sep="/"),one_file$TIME),
                               format = "%d/%m/%y %H:%M:%S", tz = "gmt") 
    
    locs<-which(trip_info$id %in% this_id) 
    for (k in 1:length(locs)){ 
      a=a+1
      start <- as.POSIXct(trip_info$leave_land[locs[k]]) 
      finish <- as.POSIXct(trip_info$backon_land[locs[k]])
      gpstrip <- one_file[one_file$POSIXct >= start & one_file$POSIXct <= finish,]
      gpstrip$trip_num <-a 
      #calculate seconds to the next point
      gpstrip$time_next_point <- c(as.numeric(difftime(gpstrip$POSIXct[2:length(gpstrip[,1])], 
                                                       gpstrip$POSIXct[1:(length(gpstrip[,1])-1)], units = "secs")), NA)

      if (a==1){
        output <-gpstrip #create data file with first set of data entry
      }
      else{
        output<-rbind(output,gpstrip) #all other data appended to file
      }
    }
    }
}

output <- output[,c("id","POSIXct","LAT","LON","HEIGHT","trip_num")]
colnames(output)[3:5] <- c("Lat","Lon","Height")

#remove errors  with Lat or Lon =0 
output<-subset(output,output$Lat!=0) 
output<-subset(output,output$Lon!=0) #

setwd("C:/Users/Jessica/Desktop/penguin analysis/11_12 analysis") 
write.csv(file = "11_12_all_gps_at_sea_Feb26.csv", output)


