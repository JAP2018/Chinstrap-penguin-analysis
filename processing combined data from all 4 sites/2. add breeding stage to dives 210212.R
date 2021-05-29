library(stringr)

setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/0 Supporting files/phenology dates") 
#--------extract 2011-12 data -------------------
dates_1112 <- read.csv("2011-12 egg dates Oct6 2019.csv")
name(dates_1112)
keep_cols <-dates_1112[c("Round","Bird","bill_depth","bill_length",
                         "start_young","end_young","start_mass_kg","end_mass_kg")] 

stage <- c()
for (i in 1:length(keep_cols[,1])){
  start <- keep_cols[i,5]
  end <- keep_cols[i,6]
  if (grepl("Eggs",start)&grepl("Eggs",end)&
      !grepl("Chicks",start)&!grepl("Chicks",end)){
    stage <- append(stage,"incubation")
  }
  else if (!grepl("Eggs",start)&!grepl("Eggs",end)&
           grepl("Chicks",start)&grepl("Chicks",end)){
    stage <- append(stage,"chick")
  }
  else if (grepl("Eggs",start)&!grepl("Chicks",start)){
    stage <- append(stage,"incubation at start")
  }
  else{
    stage <- append(stage,"incubation/chick")
  }
}

phenology_1112 <- cbind(keep_cols,stage)
phenology_1112$year <- "11_12"

#----------extract 2013-14 data -----------------
dates_1314 <- read.csv("2013-14 egg dates Oct6 2019.csv")
names(dates_1314)

keep_cols <-dates_1314[c("Round","Bird","Depth","Length",
                         "start_young","end_young","start_mass_kg","end_mass_kg")] 
   
combos<-unique(keep_cols[,c('start_young','end_young')])

stage <- c()
for (i in 1:length(keep_cols[,1])){
  start <- keep_cols[i,5]
  end <- keep_cols[i,6]
  if (start=="Creche"){ 
    stage <- append(stage,"creche")
  }
  else if (start=="CC"|start=="CX"|start=="CA"){ 
    stage <- append(stage,"chick")
  }
  else if (!grepl("C",start)&!grepl("C",end)
           &!grepl("P",start)&!grepl("P",end)){
    stage <- append(stage,"incubation")
  }
  else if (start=="EE"|start=="EX"|start=="PE"){
    stage <- append(stage,"incubation at start")
  }
  else{
    stage <- append(stage,"incubation/chick")
  }
}

phenology_1314<- cbind(keep_cols,stage)
phenology_1314$year <- "13_14"

#-------------extract dates for 2015-16 data--------------------
dates_1516<- read.csv("2015-16 egg dates Oct6 2019.csv")
names(dates_1516)

keep_cols <-dates_1516[c("Round","Bird","bill_depth","bill_length",
                         "start_young","end_young","start_mass_kg","end_mass_kg")] 

combos<-unique(keep_cols[,c('start_young','end_young')])

stage <- c()
for (i in 1:length(keep_cols[,1])){
  start <- keep_cols[i,5]
  end <- keep_cols[i,6]
  if (grepl("C",start)){ 
    stage <- append(stage,"chick")
  }
  else if (!grepl("C",start)&!grepl("C",end)){
    stage <- append(stage,"incubation")
  }
  else if (start=="EE"){
    stage <- append(stage,"incubation at start")
  }
  else{
    stage <- append(stage,"incubation/chick")
  }
}

phenology_1516<- cbind(keep_cols,stage)

phenology_1516$year <- "15_16"

#-----get stage for Signy------------------
dates_Signy<- read.csv("Signy 2015-16 egg dates Oct6 2019.csv")
names(dates_Signy)

keep_cols <-dates_Signy[c("Round","Bird","Stage")] 

stage <- c()
for (i in 1:length(keep_cols[,1])){
  if (grepl("I",keep_cols[i,3])){ 
    stage <- append(stage,"incubation")
  }
  else if (grepl("C",keep_cols[i,3])){ #guard 
    stage <- append(stage,"creche")
  }
  else {
    stage <- append(stage,"chick")
  }
}
names(keep_cols)[3] <- "start_young"
keep_cols$end_young <- NA

phenology_Signy<- cbind(keep_cols,stage)
phenology_Signy$year <- "Signy_15_16"
phenology_Signy$start_mass_kg <- NA
phenology_Signy$end_mass_kg <- NA 

#---------------------------combine all data-------------------------

all_years<- rbind(phenology_1112[c(1:2,5:10)],phenology_1314[c(1:2,5:10)],
                  phenology_1516[c(1:2,5:10)],phenology_Signy)

#need to add zeros to 13_14 and 15_16
all_years$bird_0 <- str_pad(all_years$Bird, 2, pad = "0")
all_years$Bird[all_years$year=="15_16"|all_years$year=="13_14"] <-
all_years$bird_0[all_years$year=="15_16"|all_years$year=="13_14"]

all_years$id <- paste(all_years$year,all_years$Round,all_years$Bird,sep="_")
#only need to keep penguin id and incubation or chick

all_years$stage[all_years$stage=="incubation/chick"] <- "chick"

bird_stage <- all_years[c("id","stage")]
inc_birds <- subset(bird_stage,bird_stage$stage=="incubation")
chick_birds <- subset(bird_stage,bird_stage$stage=="chick")
creche_birds <- subset(bird_stage,bird_stage$stage=="creche")
inc_start_birds <- subset(bird_stage,bird_stage$stage=="incubation at start") 

#---------------now just need to assign these to dive data.------------

setwd("C:/Users/Jessica/Desktop/Chinstrap chapter April28 2020/analysis/3 chinstrap code Sept27 2019")
dive_data<- read.csv("1. dive_daynight_4seasons_March17 2020.csv")

dive_data$stage <- NA
dive_data$stage[dive_data$id %in% inc_birds$id] <- "incubation"
dive_data$stage[dive_data$id %in% chick_birds$id] <- "guard"
dive_data$stage[dive_data$id %in% creche_birds$id] <- "creche"

dive_data$stage[dive_data$id %in% inc_start_birds$id] <- "inc at start"

write.csv(file="2. dives w stage 210212.csv",dive_data)

