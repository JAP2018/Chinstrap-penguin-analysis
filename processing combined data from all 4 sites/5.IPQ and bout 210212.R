rm(list = ls()) #clear env
x<-c("ggpubr","devtools","plyr","dplyr","ggplot2","gridExtra")
lapply(x, require, character.only = TRUE)

setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/3 chinstrap code Sept27 2019")
dives <- read.csv("4. 4season dive GPS 210212.csv")

#--------add seconds from previous dive, and previous and next dive behavior --------

all_trips <- data.frame()
for (k in unique(dives$year)){
  oneyear <- subset(dives,dives$year==k)
  for (q in unique(oneyear$trip_num)){
    onetrip <- subset(oneyear,oneyear$trip_num==q)
    
    x <- onetrip$sec2next
    onetrip$sec_from_prev<- append("none",x[-length(x)])
    
    y <- as.character(onetrip$behavior)
    onetrip$prev_behaviour <- append("none",y[-length(y)])
    onetrip$next_behaviour <- append(y[-1],"none")
    
    all_trips <- rbind(all_trips,onetrip)
  }
}

all_forage <- subset(all_trips,all_trips$behavior=="forage")

#--------------------define and add numbering system for foraging bouts --------------
a=0
with_bout <- data.frame()
all_forage$sec_from_prev <- as.numeric(all_forage$sec_from_prev)
all_forage$prev_behaviour <- as.character(all_forage$prev_behaviour)

for (k in unique(all_forage$year)){
  oneyear <- subset(all_forage,all_forage$year==k)
  for (q in unique(oneyear$trip_num)){
    onetrip <- subset(oneyear,oneyear$trip_num==q)
    
    onetrip$bout<- ifelse(((onetrip$next_behaviour=="forage"&onetrip$sec2next<=325)|#this should include all including first and last dive of each trip 
                             (onetrip$prev_behaviour=="forage"&onetrip$sec_from_prev<=325)), "yes", "no")    #if prev or next dive is foraging dive and is within 325 seconds
    
    for(i in 1:nrow(onetrip)){
      if(onetrip$bout[i]=="yes"&onetrip$prev_behaviour[i]!="forage"){
        a=a+1
        onetrip$bout[i] <- a
      }else if(onetrip$bout[i]=="yes"&onetrip$prev_behaviour[i]=="forage"){
        if(onetrip$sec_from_prev[i]>325){
          a=a+1
          onetrip$bout[i] <- a
        }else{
          onetrip$bout[i] <- a
        }
      }
    }
    with_bout <- rbind(with_bout,onetrip)
  }
}
#------------adding foraging bouts finished -----------

#------------------Get PQI---------------------------
pqi<-function(u,up_down_time){#u is dive duration and d is maximum depth 
  b=0.07527 #take median of b and c
  c=0.05907
  x=((1+b*c*exp(c*u))*(u-up_down_time))/(b*exp(c*u)+u)
  return(x)
}

with_bout$pqi<- pqi(with_bout$duration,with_bout$asc_desc_time)


#---------assign NA for IPQ for dives w surface time over 325s, and remove outliers w pqi over 5------------

with_bout$pqi[with_bout$sec2next>325] <- NA 
with_bout$pqi[with_bout$pqi>5] <- NA 

with_bout$one <- 1

with_bout$dive_num_inbout <- ifelse(with_bout$bout!="no",ave(with_bout$X, with_bout$bout, FUN = seq_along),"no")
with_bout$change_depth <- with_bout$max_depth-head(c(NA,with_bout$max_depth),-1)
with_bout$change_depth <- ifelse((with_bout$dive_num_inbout==1|with_bout$dive_num_inbout=="no"),"NA",with_bout$change_depth)

with_bout$X.1 <- NULL
with_bout$X <- NULL
write.csv(file="5.1 forage dives pqi w bout 210212.csv",with_bout)


with_bout$start_time <- as.POSIXct(with_bout$start_time)
with_bout$end_time <- with_bout$start_time+as.numeric(with_bout$duration)

has_bout <- subset(with_bout,with_bout$bout!="no")
has_bout$change_depth <- as.numeric(has_bout$change_depth)

all_bouts <- data.frame()
for (q in unique(has_bout$bout)){
  onebout <- subset(has_bout,has_bout$bout==q)
  onebout$start_time <- as.POSIXct(onebout$start_time)
  onebout$end_time <- as.POSIXct(onebout$end_time)
  
  
  this_bout_data <- data.frame(onebout[1,c("year","id","trip_num","stage","dayNight","in_out","start_mass_kg","end_mass_kg")],
                               mean(onebout$dist2colony, na.rm=TRUE),mean(onebout$ocean_depth, na.rm=TRUE),
                               nrow(onebout),mean(onebout$pqi, na.rm=TRUE),min(onebout$start_time),max(onebout$end_time),
                               mean(onebout$max_depth),mean(onebout$change_depth, na.rm=TRUE),mean(abs(onebout$change_depth), na.rm=TRUE))
  all_bouts <- rbind(all_bouts,this_bout_data)
}
colnames(all_bouts)[9:17] <- c("dist2colony","ocean_depth","num_dives","mean_ipq","bout_start","bout_end","mean_depth",
                               "mean_depthchange","mean_abs_depthchange")

all_bouts$bout_duration <- as.numeric(difftime(all_bouts$bout_end,all_bouts$bout_start,units="secs"))
write.csv(file="5.2 all bouts 210212.csv",all_bouts)

