setwd("/Users/GR/Desktop/penguin analysis/13_14 analysis") 

trip_info <- read.csv("trip_info_13_14_Dec5.csv")
master <- read.csv("GPS_Powell_Deployment_Data.csv")

setwd('/Users/GR/Desktop/penguin data/2013-2014/Powell Island/Tracking data')
a=0
for(i in 1:length(master[,1])){
  #i=1
  call_file <-as.character(master[i,1][[1]])
  one_file <-read.csv(paste(".",substr(call_file,64,nchar(call_file)),".csv",sep=""))
  #removing data points where Lat or Lon ==0 - GPS error
  one_file<-one_file[!rowSums(one_file[9] ==0),]
  one_file<-one_file[!rowSums(one_file[10] ==0),]
  
  this_id<-paste("13_14",substr(call_file,71,71),substr(call_file,78,79),sep="_")
  one_file$id <-this_id
  
  if (this_id %in% trip_info$id){ #if the penguin id is contained in file with all trips that go to sea
    #add POSIXct time to the individual gps file --> so can cut it by time in next step
    #it is gmt time
    datetime <- paste(paste(one_file$Day, one_file$Month, one_file$Year, sep = "-"), paste(one_file$Hour, one_file$Minute, one_file$Second, sep = ":"), sep = " ")
    one_file$POSIXct <- as.POSIXct(datetime, format = "%d-%m-%y %H:%M:%S", tz = "gmt")
   
    locs<-which(trip_info$id %in% this_id) #gives locations of this id in the list 
    for (k in 1:length(locs)){ 
      a=a+1
      start <- as.POSIXct(trip_info$leave_land[locs[k]]) 
      finish <- as.POSIXct(trip_info$backon_land[locs[k]])
      gpstrip <- one_file[one_file$POSIXct >= start & one_file$POSIXct <= finish,]
      gpstrip$trip_num <-a # total trip number 
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

output <- output[,c("id","POSIXct","LAT","LON","C","trip_num")]
colnames(output)[3:5] <- c("Lat","Lon","Height")
setwd("/Users/GR/Desktop/penguin analysis/13_14 analysis")
write.csv(file = "13_14_all_gps_at_sea_Dec5.csv", output)


