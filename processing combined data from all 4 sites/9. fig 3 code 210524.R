library(ggpubr)
library(ggplot2)
setwd("D:/Published papers/Chinstrap chapter April28 2020/analysis/3 chinstrap code Sept27 2019")
forage_dives <- read.csv("5.1 forage dives pqi w bout 210212.csv")
all_bouts <- read.csv(file="5.2 all bouts 210212.csv")

#-------make bott dist plot ----------
has_ipq <- subset(forage_dives,!is.na(forage_dives$pqi))

plot_1 <-ggplot(has_ipq, aes(x=bott_dist, y=pqi)) +geom_point(alpha = 1/10)+
  geom_smooth(method=lm, se=TRUE)+
  labs(x="bottom vertical distance(m)",y="IPQ of dive")+
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18))



#----------make num dive plot ----------------------
plot_2 <-ggplot(all_bouts, aes(x=num_dives, y= mean_ipq)) +geom_point(alpha = 1/10)+
  geom_smooth(method=lm, se=TRUE)+
  labs(x="number of dives in bout",y="IPQ of diving bout")+
  theme(axis.text.x = element_text(size = 16),
        axis.title.x = element_text(size = 18),
        axis.text.y = element_text(size = 16),
        axis.title.y = element_text(size = 18))


#-----------------combine plots -------------------
ggarrange(plot_1,plot_2,ncol = 2,nrow = 1)

