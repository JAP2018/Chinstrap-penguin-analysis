library(stringr)
library(ggplot2)
library(plyr)
library(raster)
library(rgeos)
library(rgdal)
library(maptools)

setwd("/Users/GR/Desktop/penguin data/signy") 
master<-read.csv(file = "chinstrap deployment.csv")
master<-master[,c("round","bird.number","Stage","Deployment.date","Recovery.date","GPS.file","TDR.file","Edited.TDR.file")]
names(master) <- c("round", "bird","stage","deploy","recover","gps_file","tdr_file","edited_tdr")


data13_14<-master[8:21,] #this is all data for 13-14
all13_14<-data.frame()
setwd("./gps13_14")
for(i in 1:length(data13_14[,1])){
  #i=1
  call_file <-as.character(data13_14[i,6][[1]])
  one_file <-read.csv(paste("B",str_sub(call_file,-18,-1),sep=''))
  one_file$id <-paste("Signy","13_14",data13_14[i,1],data13_14[i,2],sep="_")
  all13_14<-rbind(all13_14,one_file)
}
all13_14$field_season<-"13_14"

setwd("../gps15_16")
data15_16<-master[22:48,] #this is all data for 13-14
all15_16<-data.frame()
for(i in 1:length(data15_16[,1])){
  call_file <-as.character(data15_16[i,6][[1]])
  one_file <-read.csv(paste("B",str_sub(call_file,-18,-1),sep=''))
  one_file$id <-paste("Signy","15_16",data15_16[i,1],data15_16[i,2],sep="_")
  all15_16<-rbind(all15_16,one_file)
}
all15_16$Year[all15_16$Year == "2016"] <- "16" #some are 2016 for year insted of 16 - changing this to avoide problems in future
all15_16$field_season<-"15_16"

all_signy_data<-rbind(all13_14,all15_16) #all GPS data from signy, both years, uncut 

#--------------------begining to clean signy GPS data -----------------------
data<-all_signy_data

data$POSIXct <- as.POSIXct(paste(paste(data$Day,data$Month,data$Year,sep="/"),
                                 paste(sprintf("%02d", data$Hour),sprintf("%02d", data$Minute),
                                       sprintf("%02d", data$Second),sep=":")),
                           format = "%d/%m/%y %H:%M:%S", tz = "gmt") 

# Retain only those columns needed for further analyses
data <- data[,c("id","POSIXct","Lat","Lon","Height","field_season")] 
colnames(data)[2:6] <- c("dt","lat","long","alt","year")


### ADD COASTLINES
coast = readOGR("/Users/GR/Desktop/penguin analysis/Coastline_high_res_polygon", 
                "Coastline_high_res_polygon",verbose = TRUE)

### Determine what locations are over land and what locs are over sea, here for low resolution
data$pts <- data[c("long","lat")] 
coordinates(data$pts) <- ~long+lat

proj4string(data$pts) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

prj <- proj4string(coast) #calls coordinate reference system of coastline file 
data$pts <- spTransform(data$pts, CRS(prj))   

pts.land <- over(data$pts, coast)
data$land <-pts.land$surface 

## Crop the coastlines to the area used by penguins
minlat <- min(data$pts$lat)
maxlat <- max(data$pts$lat)
minlong <- min(data$pts$long)
maxlong <- max(data$pts$long)

# Create polygon with extent of study domain to clip the coastline shapefile
CP <- as(extent(minlong-5000, maxlong+5000, minlat-5000, maxlat+5000), "SpatialPolygons")
proj4string(CP) <- CRS(proj4string(coast))

## Clip the coastline shapefile
coast2 <- crop(coast, extent(minlong-5000, maxlong+5000, minlat-5000, maxlat+5000))

### Fortify spatial polygons dataframe into regular df format for use in ggplot2
coast2@data$id = rownames(coast2@data)
coast2.points = fortify(coast2, region="id")
coast2.df = join(coast2.points, coast2@data, by="id")   #from package plyr


data <- data[order(data$id,data$dt),]
data$pts <- NULL

id <- vector()
start_time <-vector()
end_time <- vector()
trip_length <- vector()
trip_complete <- vector()
leave_land <-vector()
backon_land <-vector()

for(i in 1:length(unique(data$id))){ #data is all gps files, everything, all data, even penguins that didn't go to sea on whole trial
  penguin_i <-subset(data,data$id==unique(data$id)[i]) #subset individual penguin
  penguin_i$number <- 1:nrow(penguin_i) #create column for all data (land and sea) giving column number
  at_sea_i <-subset(penguin_i, is.na(penguin_i$land)) #subset points at sea for individual penguin
  
  for (x in 1:length(at_sea_i[,1])){ #loop through all at sea points for each penguin
    if (x==1){
      start <- at_sea_i$dt[x]
      if (at_sea_i$number[1]==1){ #if first data point at sea, time of first pt at sea = last on land
        left_land<-start
      }
      else{
        left_land<-penguin_i$dt[penguin_i$number==(at_sea_i$number[x]-1)]
      }
    }
    else if (at_sea_i$number[x]!=at_sea_i$number[x-1]+1){ #if number indicates new start to at sea time
      end <- at_sea_i$dt[x-1]
      difference <- as.numeric(difftime(end,start), units = "days") #gives difference in days
      back_land<-penguin_i$dt[penguin_i$number==(at_sea_i$number[x-1]+1)]
      num_datapts<-at_sea_i$number[at_sea_i$dt==end]-at_sea_i$number[at_sea_i$dt==start]
      if (difference>(1/24) && (num_datapts>=10)){ 
        
        #excluding cases where only at sea less than 1 hour
        #excluding cases where there are less than 10 GPS points at sea
        
        id <- append(id,unique(data$id)[i])
        start_time <-append(start_time,start)
        end_time <- append(end_time,end)
        trip_length <- append(trip_length,difference) #diff between first an last point at sea
        trip_complete <- append(trip_complete,"yes") #"1" in trip_complete means yes, it is complete
        leave_land <-append(leave_land,left_land)
        backon_land <-append(backon_land,back_land)
      }
      start <- at_sea_i$dt[x]
      left_land<-penguin_i$dt[penguin_i$number==(at_sea_i$number[x]-1)]
    } 
    if (x==length(at_sea_i[,1])){ #now no control for length of last trip?? 
      trial_end <- at_sea_i$dt[x]
      difference <-as.numeric(difftime(trial_end,start), units = "days")
      back_land<-trial_end
      
      if (difference>(1/24)){ #excluding cases where only at sea less than 1 hour
        if (at_sea_i$number[x]==length(penguin_i[,1])){
          trip_complete <- append(trip_complete,"no")
        }
        else{
          trip_complete <- append(trip_complete,"yes")
        }
        id <- append(id,unique(data$id)[i])
        start_time <-append(start_time,start)
        end_time <- append(end_time,trial_end)
        trip_length <- append(trip_length,difference)
        leave_land <-append(leave_land,left_land)
        backon_land <-append(backon_land,back_land)
      }
    }
  }
}

trip_info <-cbind.data.frame(id, leave_land,start_time, end_time,backon_land,trip_length,trip_complete) 

setwd("/Users/GR/Desktop/penguin analysis/signy analysis") 
write.csv(file = "trip_info_signy_may4.csv", trip_info) 


#------------create csv of all GPS data at sea------------
a<-0 
output<-data.frame()
for (q in unique(data$id)){
  #q<- unique(data$id)[1]
  one_file<-subset(data,data$id==q)
  if (q %in% trip_info$id){ 
    locs<-which(trip_info$id %in% q) #gives locations of this id in the list
    for (k in 1:length(locs)){ 
      a<-a+1
      start <- as.POSIXct(trip_info$leave_land[locs[k]]) 
      finish <- as.POSIXct(trip_info$backon_land[locs[k]])
      gpstrip <- one_file[one_file$dt >= start & one_file$dt <= finish,]
      gpstrip$trip_num <-a # total trip number 
      #calculate seconds to the next point
      gpstrip$time_next_point <- c(as.numeric(difftime(gpstrip$dt[2:length(gpstrip[,1])], 
                                                       gpstrip$dt[1:(length(gpstrip[,1])-1)], units = "secs")), NA)
      output<-rbind(output,gpstrip)
      }
    }
}

output <- output[,c("id","dt","lat","long","alt","trip_num","year")]
colnames(output)[2:5] <- c("POSIXct","Lat","Lon","Height")

write.csv(file = "Signy_all_gps_at_sea_may4.csv", output)




