rm(list = ls())  # clear objects
graphics.off()  #close all open figures and graphics objects

x<-c("lubridate","dplyr")
lapply(x, require, character.only = TRUE)

setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/3 chinstrap code Sept27 2019")
all_info<-read.csv("0. 4year_dives_time2next_Feb24 2020.csv")

no_deep<-subset(all_info,all_info$max_depth<=200)
info_using<-subset(no_deep,no_deep$duration<=250)

#-------------------sunrise sunset stuff starts--------
setwd("C:/Users/Jessica/Desktop/Chinstrap analysis Feb8 2020/0 Supporting files/sunrise sunset")
files <- lapply(Sys.glob("sunrise*.csv"), read.csv)

for (i in 1:8){
  files[[i]]<-files[[i]][-c(1,2), ]
  colnames(files[[i]]) <- c("Day","Jan_r","Jan_s","Feb_r","Feb_s","Mar_r","Mar_s",
                       "Apl_r","Apl_s","May_r","May_s","Jun_r","Jun_s","Jul_r","Jul_s",
                       "Aug_r","Aug_s","Sep_r","Sep_s","Oct_r","Oct_s","Nov_r","Nov_s","Dec_r","Dec_s")
}
sun11<-files[[1]]
sun12<-files[[2]]
sun13<-files[[3]]
sun14<-files[[4]]
sun15<-files[[5]]
sun16<-files[[6]]
sun15_signy<-files[[7]]
sun16_signy<-files[[8]]

risetime<-c(as.character(sun11$Dec_r[20:31]),as.character(sun12$Jan_r[1:24]),#2011-12 season
            as.character(sun13$Dec_r[29:31]),as.character(sun14$Jan_r[1:31]),as.character(sun14$Feb_r[1:11]), #2013-14 season
            as.character(sun15$Dec_r[25:31]),as.character(sun16$Jan_r[1:31]),as.character(sun16$Feb_r[1:10]), #2015-16 season
            as.character(sun15_signy$Dec_r[26:31]),as.character(sun16_signy$Jan_r[1:31]),
            as.character(sun16_signy$Feb_r[1:14]))#15-16 season in Signy

settime<-c(as.character(sun11$Dec_s[20:31]),as.character(sun12$Jan_s[1:24]),#2011-12 season
           as.character(sun13$Dec_s[29:31]),as.character(sun14$Jan_s[1:31]),as.character(sun14$Feb_s[1:11]),#2013-14 season
           as.character(sun15$Dec_s[25:31]),as.character(sun16$Jan_s[1:31]),as.character(sun16$Feb_s[1:10]), #2015-16 season
           as.character(sun15_signy$Dec_s[26:31]),as.character(sun16_signy$Jan_s[1:31]),
           as.character(sun16_signy$Feb_s[1:14]))#15-16 season in Signy


rise_time<-paste(substr(risetime,1,2),substr(risetime,3,4),"00",sep=":")
set_time<-paste(substr(settime,1,2),substr(settime,3,4),"00",sep=":")

date<-c(paste("2011","12",20:31,sep="/"),paste("2012","1",1:24,sep="/"),
        paste("2013","12",29:31,sep="/"),paste("2014","1",1:31,sep="/"),paste("2014","2",1:11,sep="/"),
        paste("2015","12",25:31,sep="/"),paste("2016","1",1:31,sep="/"),paste("2016","2",1:10,sep="/"),
        paste("2015","12",26:31,sep="/"),paste("2016","1",1:31,sep="/"),paste("2016","2",1:14,sep="/"))

sunRise<-as.POSIXct(paste(date, rise_time, sep = " "),format = "%Y/%m/%d %H:%M:%S", tz = "UTC")
sunSet<-as.POSIXct(paste(date, set_time, sep = " "),format = "%Y/%m/%d %H:%M:%S", tz = "UTC")

all_sun <- data.frame(Date = as.Date(sunRise, tz = 'EST'), sunRise = sunRise, sunSet = sunSet)
#Signy is gone now.Because signy's being added later 


sun_no_signy<-all_sun[1:129,]
dives_no_signy<-subset(info_using,info_using$year!="Signy_15_16")

dives_no_signy$start_time <- as.POSIXct(dives_no_signy$start_time)
dives_no_signy$Date <- as.Date(dives_no_signy$start_time, tz = 'UTC')

dives_no_signy<-inner_join(dives_no_signy,sun_no_signy)

dives_no_signy$sunset_char<-as.character(dives_no_signy$sunSet)


dayNight<-c()
for (i in 1:length(dives_no_signy[,1])){
  if (substr(dives_no_signy$sunset_char[i],12,13)=="00"){
    day_night<-ifelse(dives_no_signy$start_time[i] < dives_no_signy$sunRise[i] & 
                        dives_no_signy$start_time[i] > dives_no_signy$sunSet[i], 'night', 'day')
  }
  else{
    day_night <- ifelse(dives_no_signy$start_time[i] > dives_no_signy$sunRise[i] & 
                          dives_no_signy$start_time[i] < dives_no_signy$sunSet[i], 'day', 'night')
  }
  dayNight<-append(dayNight,day_night)
}
dives_no_signy$dayNight<-dayNight

rise<-as.POSIXct(dives_no_signy$sunRise)
rise_time<- as.POSIXct(paste(hour(rise), minute(rise),second(rise)), format = "%H %M %S") 
min(rise_time) #earliest sunrise 05:22:00 GMT
max(rise_time) #latest sunrise 07:21:00 GMT

set<-as.POSIXct(dives_no_signy$sunSet)
set_time<- as.POSIXct(paste(hour(set), minute(set),second(set)), format = "%H %M %S") 

#-------------------now add Signy 15_16 data --------------------------

#redefine for Signy
sun_signy<-all_sun[130:length(all_sun[,1]),]
dives_signy<-subset(info_using,info_using$year=="Signy_15_16")

dives_signy$start_time <- as.POSIXct(dives_signy$start_time)
dives_signy$Date <- as.Date(dives_signy$start_time, tz = 'UTC')

dives_signy<-inner_join(dives_signy,sun_signy)
dives_signy$sunset_char<-as.character(dives_signy$sunSet)

dayNight<-c()
for (i in 1:length(dives_signy[,1])){
  if (substr(dives_signy$sunset_char[i],12,13)=="00"){
    day_night<-ifelse(dives_signy$start_time[i] < dives_signy$sunRise[i] & 
                        dives_signy$start_time[i] > dives_signy$sunSet[i], 'night', 'day')
  }
  else{
    day_night <- ifelse(dives_signy$start_time[i] > dives_signy$sunRise[i] & 
                          dives_signy$start_time[i] < dives_signy$sunSet[i], 'day', 'night')
  }
  dayNight<-append(dayNight,day_night)
}
dives_signy$dayNight<-dayNight

rise<-as.POSIXct(dives_signy$sunRise)
rise_time<- as.POSIXct(paste(hour(rise), minute(rise),second(rise)), format = "%H %M %S") 
min(rise_time) #earliest sunrise 05:22:00 GMT
max(rise_time) #latest sunrise 07:21:00 GMT

set<-as.POSIXct(dives_signy$sunSet)
set_time<- as.POSIXct(paste(hour(set), minute(set),second(set)), format = "%H %M %S") 

four_seasons<-rbind(dives_no_signy,dives_signy)

setwd("C:/Users/Jessica/Desktop/Chinstrap analysis Feb8 2020/3 chinstrap code Sept27 2019")
write.csv(file = "1. dive_daynight_4seasons_March17 2020.csv", four_seasons)  
