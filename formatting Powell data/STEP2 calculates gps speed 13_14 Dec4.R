library(ggplot2)
library(plyr)
library(raster)
library(rgeos)
library(rgdal)
library(maptools)

setwd("/Users/GR/Desktop/penguin analysis/13_14 analysis") 
data <- read.csv("13_14_all_gps_at_sea_Dec5.csv")

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

list_dist <- list()
list_time_diff <- list()

colony <- c(-46.0601, -60.6039)

# here, we take each bird's track and calculate the distances between consecutive GPS points
# and also the difference in time (seconds) between consecutive fixes


for(i in 1:length(unique(data$trip_num))){ 
  print(i)
  tmp <- data[data$trip_num == unique(data$trip_num)[i],]
  n <- nrow(tmp)
  list_dist[[i]] <- c(gcd.hf(tmp$Lon[1:(n-1)], tmp$Lat[1:(n-1)], tmp$Lon[2:n], tmp$Lat[2:n])*1000, NA)
  # time difference between consecutive points
  list_time_diff[[i]] <- c(as.numeric(difftime(tmp$POSIXct[2:n], tmp$POSIXct[1:(n-1)], units = "secs")), NA)
print(n == length(list_dist[[i]]))
} # distance in meters
# we use this to calculate speed in meters/second
# add to main data frame
data$dist_next_point <- do.call("c", list_dist)
data$time_next_point <- do.call("c", list_time_diff)
# calc speed
data$speed2next <- data$dist_next_point/data$time_next_point
# find distance to colony
data$dist2colony <- gcd.hf(data$Lon, data$Lat, colony[1], colony[2])


setwd("/Users/GR/Desktop/penguin analysis/13_14 analysis")
write.csv(file = "13_14_gpssea_speed_Dec5.csv", data)  

