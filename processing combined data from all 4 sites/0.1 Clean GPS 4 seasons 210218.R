setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/2 4 island data/4 season org GPS") 

gps_1112 <- read.csv(list.files()[1])
gps_1314 <- read.csv(list.files()[2])
gps_1516 <- read.csv(list.files()[3])
signy_gps <- read.csv(list.files()[4])


#==================================Signy=======================================
#add speed from previous dive
all_trips <- data.frame()
for (q in unique(signy_gps$trip_num)){
  onetrip <- subset(signy_gps,signy_gps$trip_num==q)
  x <- onetrip$speed2next
  onetrip$speed_from_prev<- append("none",x[-length(x)])
  all_trips <- rbind(all_trips,onetrip)
}

#removed points where speed from prev and to next are both >10m/s 
less10 <- subset(all_trips,all_trips$speed2next<=10|all_trips$speed_from_prev<=10)
nrow(less10)

#remove other GPS point identified through visual inspection

r1 <- subset(less10,less10$X.1!=34134)#remove error from trip 61
r2 <- subset(r1,r1$X.1!=34710)#remove error from trip 63
clean<- subset(r2,r2$X.1!=36024)#remove error from trip 69

setwd("C:/Users/Jessica/Desktop/Chinstrap analysis Feb8 2020/2 4 island data/4 season org clean GPS March 15 2020")
write.csv(file="Signy_org_gps_erros_removed_March15 2020.csv",clean)


#=============================11_12==================================================
#add speed from previous dive
all_trips <- data.frame()
for (q in unique(gps_1112$trip_num)){
  onetrip <- subset(gps_1112,gps_1112$trip_num==q)
  x <- onetrip$speed2next
  onetrip$speed_from_prev<- append("none",x[-length(x)])
  all_trips <- rbind(all_trips,onetrip)
}

#removed points where speed from prev and to next are both >10m/s 
less10 <- subset(all_trips,all_trips$speed2next<=10|all_trips$speed_from_prev<=10)

r1 <- subset(less10,less10$X.1!=24781)#remove error from trip 140
r2 <- subset(r1,r1$X.1!=23202)#remove error from trip 116
clean<- subset(r2,r2$X.1!=22068)#remove error from trip 100

write.csv(file="1112_org_gps_erros_removed_March15 2020.csv",clean)


#=============================13_14==================================================

#add speed from previous dive
all_trips <- data.frame()
for (q in unique(gps_1314$trip_num)){
  onetrip <- subset(gps_1314,gps_1314$trip_num==q)
  x <- onetrip$speed2next
  onetrip$speed_from_prev<- append("none",x[-length(x)])
  all_trips <- rbind(all_trips,onetrip)
}

#removed points where speed from prev and to next are both >10m/s 
less10 <- subset(all_trips,all_trips$speed2next<=10|all_trips$speed_from_prev<=10)


remove <- c("30509","28485","28290","26158","26191","25376","25370","25284","25277",
            "25265","24874","24810","23538","21860","12853","13275","13882","22323",
            "22684","24252","31265","26672","25490","13483","5664")
#from trips: 207, 183, 178,156*2,145*2,142*2,141,137,136,120,98,24,29,36,
#110,106,29,36,133,211,162,147,31,6

clean <- less10 [! less10$X.1 %in% remove,]

# for (i in unique(less10$trip_num)){
#   onetrip <- subset(clean,clean$trip_num==i)
#   
#   #remove first and last point to make error id easier
#   onetrip <- onetrip[-nrow(onetrip),]
#   onetrip <- onetrip[-1,]
#   plot(onetrip$Lon, onetrip$Lat, type='p',pch=1,main=paste("1314","trip",i))
#   lines(onetrip$Lon, onetrip$Lat, type='l')
# }

write.csv(file="1314_org_gps_erros_removed_March15 2020.csv",clean)


#=============================15_16 doesn't have speed 2 next==================================================

#add speed from previous dive
all_trips <- data.frame()
for (q in unique(gps_1516$trip_num)){
  onetrip <- subset(gps_1516,gps_1516$trip_num==q)
  x <- onetrip$speed2next
  onetrip$speed_from_prev<- append("none",x[-length(x)])
  all_trips <- rbind(all_trips,onetrip)
}

#removed points where speed from prev and to next are both >10m/s 
less10 <- subset(all_trips,all_trips$speed2next<=10|all_trips$speed_from_prev<=10)
nrow(less10)

remove <- c("13866","10834","10645","10239","10838","9623")
#from trips:62,50,46,37,50,33
clean <- less10 [! less10$X.1 %in% remove,]


for (i in 1:32){
    
  onetrip <- subset(clean,clean$trip_num==i)
  onetrip <- onetrip[-nrow(onetrip),]
  onetrip <- onetrip[-1,]
  plot(onetrip$Lon, onetrip$Lat, type='p',pch=1,main=paste("1516","trip",i))
  lines(onetrip$Lon, onetrip$Lat, type='l')
}

write.csv(file="1516_org_gps_erros_removed_March15 2020.csv",clean)

