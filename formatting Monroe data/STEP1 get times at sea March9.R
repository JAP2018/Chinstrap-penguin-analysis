library(ggplot2)
library(plyr)
library(raster)
library(rgeos)
library(rgdal)
library(maptools)

setwd("C:/Users/Jessica/Desktop/penguin analysis/15_16 analysis") 
master <- read.csv("Jessy_edits_GPS_Monroe_Deployment_Data.csv")

setwd('C:/Users/Jessica/Desktop/penguin data/2015-2016/Monroe Island/Tracking data')
for(i in 1:length(master[,1])){
  call_file <-as.character(master[i,1][[1]])
  one_file <-read.csv(paste(".",substr(call_file,69,nchar(call_file)),".csv",sep=""))
  one_file$id <-paste("15_16",substr(call_file,76,76),substr(call_file,83,84),sep="_")
  if (i==1){
    data <-one_file
  }
  else{
    data<-rbind(data,one_file)
  }
}

data$POSIXct <- as.POSIXct(paste(paste(data$Day, data$Month, data$Year, sep = "/"),
                                 paste(data$Hour, data$Minute, data$Second, sep = ":")), 
                           format = "%d/%m/%y %H:%M:%S", tz = "gmt") 

# Retain only those columns needed for further analyses
data <- data[,c("id","POSIXct","Lat","Lon","Height")] 
colnames(data)[2:5] <- c("dt","lat","long","alt")

### ADD COASTLINES
coast = readOGR("C:/Users/Jessica/Desktop/penguin analysis/Coastline_high_res_polygon/", #ME: make sure have last dash at end otherwise error saying cannot gind .dbf file even though it's in the same folder
                "Coastline_high_res_polygon",verbose = TRUE)

### Determine what locations are over land and what locs are over sea
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


for(i in 1:length(unique(data$id))){ #data is all gps files, everything, all data, even penguins that did'nt go to sea on whole trial
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
      if (difference>(1/24) && #excluding cases where only at sea less than 1 hour
          at_sea_i$number[at_sea_i$dt==end]-at_sea_i$number[at_sea_i$dt==start]>=10){ 
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
    if (x==length(at_sea_i[,1])){ 
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

setwd("/Users/GR/Desktop/penguin analysis/")
write.table(trip_info,file="trip_info_15_16_Dec11.csv", sep = ",",row.names = FALSE)

