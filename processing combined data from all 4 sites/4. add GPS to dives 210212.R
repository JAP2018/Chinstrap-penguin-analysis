rm(list = ls())  # clear objects
graphics.off()  #close all open figures and graphics objects

x<-c("signal", "lubridate","ggplot2","plyr","raster","rgeos","rgdal","maptools","adehabitatLT","foreach", "ggmap","dismo","ggpubr")
lapply(x, require, character.only = TRUE)

#------------calling 4 GPS files & 1 dive file -------------
setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/2 4 island data/4 season org clean GPS March 15 2020")
D<-list()
D[[1]] <- read.csv(list.files()[1])
D[[2]] <- read.csv(list.files()[2])
D[[3]] <- read.csv(list.files()[3])
D[[4]] <- read.csv(list.files()[4])


setwd("../../3 chinstrap code Sept27 2019") 
allyear_dives<- read.csv("3. 4season_dive_behaviour 210212.csv")
nrow(allyear_dives) #237805 dives at start Feb12 2021

# this function calculates distance between the arguments assuming that earth is a sphere
gcd.hf <- function(long1, lat1, long2, lat2) { 
  R <- 6371 # Earth mean radius [km]
  deg2rad <- function(deg) return(deg*pi/180)
  long1 <- deg2rad(long1)
  long2 <- deg2rad(long2)
  lat1 <- deg2rad(lat1)
  lat2 <- deg2rad(lat2)
  delta.long <- (long2 - long1)
  delta.lat <- (lat2 - lat1)
  a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
  c <- 2 * asin(min(1,sqrt(a)))
  
  coo2 <- function(x){2 * asin(min(1,sqrt(x)))}
  c <- lapply(a, coo2)
  c <- do.call("c", c)
  d = R * c
  return(d) # Distance in km
}

#-------------colony coordinates for 4 seasons--------
colony<-list()
colony[[1]] <- c(-44.589999, -60.683468)
colony[[2]] <- c(-45.02309, -60.72775) 
colony[[3]] <- c(-46.0601, -60.6039)
colony[[4]] <- c(-45.595359, -60.708532)


#--------align signy trip num to tdr ----------------------
D[[4]]$trip_num<-D[[4]]$trip_num-19

D[[4]]$trip_num[D[[4]]$trip_num>17] <- 
  as.numeric(D[[4]]$trip_num[D[[4]]$trip_num>17])-1

D[[4]] <- subset(D[[4]],D[[4]]$trip_num!=57)

D[[4]] $id <- as.character(D[[4]] $id)

D[[4]] $trip_num[D[[4]] $trip_num>15&D[[4]] $trip_num<32] <- 
  as.numeric(D[[4]] $trip_num[D[[4]] $trip_num>15&D[[4]] $trip_num<32])+27

D[[4]] $trip_num[nchar(D[[4]] $id)==15&D[[4]] $trip_num>31&D[[4]] $trip_num<57] <- 
  as.numeric(D[[4]] $trip_num[nchar(D[[4]] $id)==15&D[[4]] $trip_num>31
                              &D[[4]] $trip_num<57])-15

D[[4]] $trip_num[substr(D[[4]] $id,13,13)=="3"] <- 
  as.numeric(D[[4]] $trip_num[substr(D[[4]] $id,13,13)=="3"])+2

D[[4]] $trip_num[D[[4]] $trip_num>44&D[[4]] $trip_num<58] <- 
  as.numeric(D[[4]] $trip_num[D[[4]] $trip_num>44&D[[4]] $trip_num<58])+1


D[[4]]$trip_num[D[[4]]$trip_num==58&D[[4]] $id=="Signy_15_16_2_2"] <- 16

D[[4]]$trip_num[D[[4]]$trip_num>=60] <- as.numeric(D[[4]]$trip_num[D[[4]]$trip_num>=60])-1

#----------------align signy trip num to tdr finished --------------------------------


#-----------------remove trips that have <10 GPS pts----------------------
number<-c()
for (i in unique(D[[2]]$trip_num)){ 
  one_trip<-subset(D[[2]],D[[2]]$trip_num==i)
  if (length(one_trip[,1])<10){
    print(i)
    D[[2]]<-D[[2]][!D[[2]]$trip_num==i,]
  }
  number<-append(number,length(one_trip[,1]))
}


number<-c()
for (i in unique(D[[4]]$trip_num)){ 
  one_trip<-subset(D[[4]],D[[4]]$trip_num==i)
  if (length(one_trip[,1])<10){
    print(i)
    D[[4]]<-D[[4]][!D[[4]]$trip_num==i,]
  }
  number<-append(number,length(one_trip[,1]))
}


#--------------------start the loop ==================
oneyear_dives<-list()
final<-list()
onemin_gps_clean <- data.frame()
rows_org_gps <- c()
all_trip_dist <- data.frame()

for (k in 1:4){
  oneyear_dives[[k]]<-subset(allyear_dives,allyear_dives$year==unique(allyear_dives$year)[k])
  
  trips_canuse<-intersect(unique(D[[k]]$trip_num),unique(oneyear_dives[[k]]$trip_num))
  
  final[[k]]<-data.frame()
  for(i in trips_canuse){
    print(i)
    one_trip<-subset(D[[k]],D[[k]]$trip_num==i)
    one_tdr<-subset(oneyear_dives[[k]],oneyear_dives[[k]]$trip_num==i)
    
    #-----------------make dataset of max dist to colony and trip duration-----------
    max_dist <- max(one_trip$dist2colony)
    time_last_dive <- one_tdr$start_time[nrow(one_tdr)]
    
    this_trip_dist <- data.frame(one_tdr[1,c("year","id","trip_num","stage","start_time")],time_last_dive,max_dist)

    colnames(this_trip_dist)[5] <- c("time_first_dive")
    all_trip_dist <- rbind(all_trip_dist,this_trip_dist)
    #-----------------------------dataset making end-----------------------------------
    
    
    #=======================start to remove parts with >1 hour without data =====================
    one_trip$POSIXct <- as.POSIXct(one_trip$POSIXct)
    start_time<-c()
    end_time<-c()
    time_diff<-one_trip$time_2_next
    time_ok<-3600 #1 hour
    
    for (q in 1:(length(time_diff)-1)){ #last time diff is NA, so need to be one less
      if ((time_diff[1]<=time_ok) & (q==1)){ 
        start_time<-append(start_time,as.POSIXct(one_trip$POSIXct[1]))
      }
      if (q>1){
        if ((time_diff[q]<=time_ok) & (time_diff[q-1]>time_ok)){
          start_time<-append(start_time,as.POSIXct(one_trip$POSIXct[q]))
        }
      }
      if (!is.na(time_diff[q+1])){
        if (time_diff[q]<=time_ok & time_diff[q+1]>time_ok){ #if time to next point >1 h
          end_time<-append(end_time,as.POSIXct(one_trip$POSIXct[q+1]))
        }
      }
      if (time_diff[q]<=time_ok & is.na(time_diff[q+1]) ){ #if time to next point >1 h
        end_time<-append(end_time,as.POSIXct(one_trip$POSIXct[q+1]))
      }
    }
    win_1h<-interval(start_time,end_time)
    data_canuse<-foreach (z=1:length(win_1h), .combine='rbind') %do% {
      timebin_z<-one_tdr[as.POSIXct(one_tdr$start_time) %within% win_1h[z],]
    }

    # #=======================removing >1 hour without data ENDS=====================
    if(nrow(data_canuse)>0){
      tdr_time<-as.POSIXct(data_canuse$start_time)
      GPS_time <- as.POSIXct(one_trip$POSIXct)
      
      Lon = pchip(as.numeric(GPS_time), one_trip$Lon, as.numeric(tdr_time)) 
      Lat = pchip(as.numeric(GPS_time), one_trip$Lat, as.numeric(tdr_time))

      dive_w_GPS <- data.frame(data_canuse,Lon,Lat)
      final[[k]]<-rbind(final[[k]],dive_w_GPS)
      
     }
  }
  
  final[[k]]$dist2colony <- gcd.hf(final[[k]]$Lon, final[[k]]$Lat, 
                                   colony[[k]][1], colony[[k]][2])
  
}

setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/3 chinstrap code Sept27 2019")
write.csv(file="4. trip max dist duration 210212.csv",all_trip_dist)


dive_data<-rbind(final[[1]],final[[2]],final[[3]],final[[4]])

write.csv(file = "4. 4season dive GPS 210212.csv", dive_data)
