if(!is.null(dev.list())) dev.off()# Clear plots
cat("\014") # Clear console
rm(list=ls())# Clean workspace
library("ggplot2")
library("ggpubr")
#----------does IPQ go down over time??? - no it goes up-------------------
all_bouts <- read.csv(file="5.2 all bouts 210212.csv")
trip_info <- read.csv("4. trip max dist duration 210212.csv")
trip_info$time_first_dive <- as.POSIXct(trip_info$time_first_dive)
trip_info$time_last_dive <- as.POSIXct(trip_info$time_last_dive)
trip_info$trip_duration <- difftime(trip_info$time_last_dive,trip_info$time_first_dive,units="days") 

#-------------align by time from inc end -----------------------------
all_nearbouts <- data.frame()
for (i in unique(trip_info$year)){
  oneyear_shorttrips <- subset(trip_info,trip_info$year==i&trip_info$trip_duration<1)
  cutoff <- median(oneyear_shorttrips$max_dist)
  nearbouts <- subset(all_bouts,all_bouts$year==i&all_bouts$dist2colony<cutoff)#bouts within median trip distance (<1 day)

  #-------------add time to inc end ---------
  onyear_inc <- subset(trip_info,trip_info$year==i&trip_info$stage=="incubation")
  nearbouts$time_to_incend <- as.numeric(difftime(as.POSIXct(nearbouts$bout_start), 
                                                  max(onyear_inc$time_last_dive),units = "days"))
  all_nearbouts <- rbind(all_nearbouts,nearbouts)
}

names(all_nearbouts)[2] <- "colony"

time_plot <- ggplot(all_nearbouts, aes(x=time_to_incend, y=mean_ipq,color=colony)) +geom_point(alpha = 2/10)+
  geom_smooth(method = "nls", formula = y ~ a * x + b, se = F,
              method.args = list(start = list(a = 0.1, b = 0.1)))+
  labs(x="days from end of incbation",y="IPQ of bout near colony")+
  theme(text = element_text(size=20),
        plot.margin = margin(5.5,10, 5.5, 5.5))


#---------dist to colony plot --------
dist_plot <- ggplot(data = all_bouts, aes(x=dist2colony, y= mean_ipq)) + 
  geom_point(alpha = 1/10)+ geom_smooth(method=lm, se=TRUE)+
  labs(x="distance to colony (km)",y="bout IPQ")+
  theme(text = element_text(size=20))

#-----------------combine plots -------------------
ggarrange(dist_plot,time_plot,ncol = 2,nrow = 1,labels = c("a)","b)"),
          font.label = list(size = 20),common.legend = TRUE, legend="right")




#--------------------IPQ near colony over time mixed model mixed model -------------------
julien_date <- as.numeric(format(as.Date(all_nearbouts$bout_start),"%j"))
all_nearbouts$julien_date <- ifelse(julien_date<100,julien_date+365,julien_date)

lmm <- lmer(sqrt(mean_ipq) ~ julien_date + year + (1 | id), data = all_nearbouts)
summary(lmm)
anova(lmm)

lmm0 <- lmer(sqrt(mean_ipq) ~ 1 + year + (1 | id), data = all_nearbouts)
anova(lmm,lmm0) 

lmm02 <- lmer(sqrt(mean_ipq) ~ julien_date + 1 + (1 | id), data = all_nearbouts) 
summary(lmm02)

anova(lmm,lmm02) 
