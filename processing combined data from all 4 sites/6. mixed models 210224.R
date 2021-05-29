library(lme4)
setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/3 chinstrap code Sept27 2019")
forage_dives <- read.csv("5.1 forage dives pqi w bout 210212.csv")
all_bouts <- read.csv(file="5.2 all bouts 210212.csv")
trip_info <- read.csv("4. trip max dist duration 210212.csv")

std <- function(x) sd(x)/sqrt(length(x))

#-------bout vs. no bout------------------------
has_ipq <- subset(forage_dives,!is.na(forage_dives$pqi))
has_ipq$y_n_bout<- ifelse(has_ipq$bout=="no", "no", "bout")
has_ipq$y_n_bout <- as.factor(has_ipq$y_n_bout)

in_bout <- subset(has_ipq,has_ipq$y_n_bout=="bout")
not_in_bout <- subset(has_ipq,has_ipq$y_n_bout=="no")

lmm <- lmer(sqrt(pqi) ~ y_n_bout + year + (1 | id), data = has_ipq)
summary(lmm)
anova(lmm)

lmm0 <- lmer(sqrt(pqi) ~ 1 + year + (1 | id), data = has_ipq)
anova(lmm,lmm0) 

lmm02 <- lmer(sqrt(pqi) ~ y_n_bout + 1 + (1 | id), data = has_ipq)


#------------is the an association between the number of dives and IPQ? -----------------------
lmm <- lmer(sqrt(mean_ipq) ~ num_dives + year + (1 | id), data = all_bouts)
summary(lmm)
anova(lmm)

lmm0 <- lmer(sqrt(mean_ipq) ~ 1 + year + (1 | id), data = all_bouts)
anova(lmm,lmm0) 

lmm02 <- lmer(sqrt(mean_ipq) ~ num_dives + 1 + (1 | id), data = all_bouts)
summary(lmm02)

#-------bottom dist vs. IPQ------------------------
has_ipq <- subset(forage_dives,!is.na(forage_dives$pqi))
lmm <- lmer(sqrt(pqi) ~ bott_dist + year + (1 | id), data = has_ipq)
summary(lmm)
anova(lmm)

lmm0 <- lmer(sqrt(pqi) ~ 1 + year + (1 | id), data = has_ipq)
anova(lmm,lmm0) 

lmm02 <- lmer(sqrt(pqi) ~ bott_dist + 1 + (1 | id), data = has_ipq)
summary(lmm02)


#==========================below need to look at effect of colony===================

#------------Do bouts further from colony have higher IPQ?-----------------------
lmm <- lmer(sqrt(mean_ipq) ~ dist2colony + year + (1 | id), data = all_bouts)
summary(lmm)
anova(lmm)

lmm0 <- lmer(sqrt(mean_ipq) ~ 1 + year + (1 | id), data = all_bouts)
anova(lmm,lmm0) 

lmm02 <- lmer(sqrt(mean_ipq) ~ dist2colony + 1 + (1 | id), data = all_bouts)
summary(lmm02)

anova(lmm,lmm02) 

# -----------------IPQ w dist to colony during incubation-------------

only_inc <- subset(all_bouts,all_bouts$stage=="incubation")
lmm <- lmer(sqrt(mean_ipq) ~ dist2colony + year + (1 | id), data = only_inc)
summary(lmm)
anova(lmm)

lmm0 <- lmer(sqrt(mean_ipq) ~ 1 + year + (1 | id), data = only_inc)
anova(lmm,lmm0) 

lmm02 <- lmer(sqrt(mean_ipq) ~ dist2colony + 1 + (1 | id), data = only_inc)
summary(lmm02)

anova(lmm,lmm02)


for (i in unique(only_inc$year)){
  one_colony <-subset(only_inc,only_inc$year==i) 
  
  print(i)
  lmm <- lmer(sqrt(mean_ipq) ~ dist2colony + (1 | id), data = one_colony)
  lmm0 <- lmer(sqrt(mean_ipq) ~ 1 + (1 | id), data = one_colony)
  print(anova(lmm,lmm0))
  print(summary(lmm))
  
}

# -----------------IPQ w dist to colony during guard-------------

only_chick <- subset(all_bouts,all_bouts$stage=="guard")
lmm <- lmer(sqrt(mean_ipq) ~ dist2colony + year + (1 | id), data = only_chick)
summary(lmm)
anova(lmm)

lmm0 <- lmer(sqrt(mean_ipq) ~ 1 + year + (1 | id), data = only_chick)
anova(lmm,lmm0) 

lmm02 <- lmer(sqrt(mean_ipq) ~ dist2colony + 1 + (1 | id), data = only_chick)
summary(lmm02)

anova(lmm,lmm02) 

#-----------------do birds dive to shallower depths in inc than in guard for foraging dives ------------------
no_incstart <- subset(forage_dives,forage_dives$stage!="inc at start")
this_data <- no_incstart
this_data$stage <- ifelse(this_data$stage=="incubation","incubation","chick")

inc_depth<- subset(this_data,this_data$stage=="incubation")$max_depth
chick_depth<- subset(this_data,this_data$stage=="chick")$max_depth


lmm <- lmer(max_depth ~ stage + year + (1 | id), data = this_data)
summary(lmm)
anova(lmm)

lmm0 <- lmer(max_depth ~ 1 + year + (1 | id), data = this_data)
anova(lmm,lmm0)

lmm02 <- lmer(max_depth ~ stage + 1 + (1 | id), data = this_data)
anova(lmm,lmm02) 

all_inc_depth <- c()
all_chick_depth <- c()
all_std_inc <- c()
all_std_chick <- c()

for (i in unique(this_data$year)){
  one_colony <-subset(this_data,this_data$year==i) 
  
  inc_depth<- subset(one_colony,one_colony$stage=="incubation")$max_depth
  chick_depth<- subset(one_colony,one_colony$stage=="chick")$max_depth
  all_inc_depth <- append(all_inc_depth,round(mean(inc_depth),digits=2))
  all_std_inc <- append(all_std_inc,round(std(inc_depth),digits=2))
  
  all_chick_depth <- append(all_chick_depth,round(mean(chick_depth),digits=2))
  all_std_chick <- append(all_std_chick,round(std(chick_depth),digits=2))
  
  
  print(i)
  lmm <- lmer(max_depth ~ stage + (1 | id), data = one_colony)
  lmm0 <- lmer(max_depth ~ 1 + (1 | id), data = one_colony)
  print(anova(lmm,lmm0))
  print(summary(lmm))
  
}
cbind(unique(this_data$year),all_inc_depth,all_std_inc,all_chick_depth,all_std_chick)

#----------------are max distance of trips different in diff stages?? ---------------------
no_incstart <- subset(trip_info,trip_info$stage!="inc at start")
my_data <- no_incstart
my_data$stage <- ifelse(my_data$stage=="incubation","incubation","chick")


inc_dist<- subset(my_data,my_data$stage=="incubation")$max_dist
chick_dist<- subset(my_data,my_data$stage=="chick")$max_dist

lmm <- lmer(max_dist ~ stage + year + (1 | id), data = my_data)
summary(lmm)
anova(lmm)

lmm0 <- lmer(max_dist ~ 1 + year + (1 | id), data = my_data)
anova(lmm,lmm0) 

lmm02 <- lmer(max_dist ~ stage + 1 + (1 | id), data = my_data)
anova(lmm,lmm02) 

all_inc_trip <- c()
all_std_inc <- c()
all_chick_trip <- c()
all_std_chick <- c()

for (i in unique(my_data$year)){
  one_colony <-subset(my_data,my_data$year==i) 
  
  inc_trip<- subset(one_colony,one_colony$stage=="incubation")$max_dist
  chick_trip<- subset(one_colony,one_colony$stage=="chick")$max_dist
  
  all_inc_trip <- append(all_inc_trip,round(mean(inc_trip),digits=2))
  all_std_inc <- append(all_std_inc,round(std(inc_trip),digits=2))
  
  all_chick_trip <- append(all_chick_trip,round(mean(chick_trip),digits=2))
  all_std_chick <- append(all_std_chick,round(std(chick_trip),digits=2))
  
  print(i)
  if(i!="Monroe"){
    lmm <- lmer(max_dist ~ stage + (1 | id), data = one_colony)
    lmm0 <- lmer(max_dist ~ 1 + (1 | id), data = one_colony)
    
    print(anova(lmm,lmm0))
    print(summary(lmm))
          
  }else{
    lmm <- lm(max_dist ~ stage , data = one_colony) 
    print(summary(lmm)) 
  }
}
cbind(unique(my_data$year),all_inc_trip,all_std_inc,all_chick_trip,all_std_chick)



#----------------do distance of foraging trips get shorter later in incubation? ---------------------
inc_trips <- subset(trip_info,trip_info$stage=="incubation")
julien_date <- as.numeric(format(as.Date(inc_trips$time_first_dive),"%j"))

inc_trips$julien_date <- ifelse(julien_date<30,julien_date+365,julien_date)

lmm <- lmer(max_dist ~ julien_date + year + (1 | id), data = inc_trips)
summary(lmm)
anova(lmm)

lmm0 <- lmer(max_dist ~ 1 + year + (1 | id), data = inc_trips)
anova(lmm,lmm0) 

lmm02 <- lmer(max_dist ~ julien_date + 1 + (1 | id), data = inc_trips)
summary(lmm02)

anova(lmm,lmm02) 

for (i in unique(inc_trips$year)){
  one_colony <-subset(inc_trips,inc_trips$year==i) 
  
  print(i)
  if(i!="Monroe"){
    lmm <- lmer(max_dist ~ julien_date + (1 | id), data = one_colony)
    lmm0 <- lmer(max_dist ~ 1 + (1 | id), data = one_colony)
    
    print(anova(lmm,lmm0))
    print(summary(lmm))
    
  }else{
    lmm <- lm(max_dist ~ julien_date , data = one_colony) 
    print(summary(lmm)) 
  }
}


#----------------do distance of foraging trips get shorter later in guard? ---------------------
chick_trips <- subset(trip_info,trip_info$stage=="guard")
julien_date <- as.numeric(format(as.Date(chick_trips$time_first_dive),"%j"))

chick_trips$julien_date <- ifelse(julien_date<30,julien_date+365,julien_date)

lmm <- lmer(max_dist ~ julien_date + year + (1 | id), data = chick_trips)
summary(lmm)
anova(lmm)

lmm0 <- lmer(max_dist ~ 1 + year + (1 | id), data = chick_trips)
anova(lmm,lmm0) 

lmm02 <- lmer(max_dist ~ julien_date + 1 + (1 | id), data = chick_trips)
summary(lmm02)

anova(lmm,lmm02) 

for (i in unique(chick_trips$year)){
  one_colony <-subset(chick_trips,chick_trips$year==i) 
  
  print(i)
  if(i!="Monroe"){
    lmm <- lmer(max_dist ~ julien_date + (1 | id), data = one_colony)
    lmm0 <- lmer(max_dist ~ 1 + (1 | id), data = one_colony)
    
    print(anova(lmm,lmm0))
    print(summary(lmm))
    
  }else{
    lmm <- lm(max_dist ~ julien_date , data = one_colony) 
    print(summary(lmm)) 
  }
}

