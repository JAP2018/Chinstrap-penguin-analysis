x<-c("ggpubr","devtools","plyr","dplyr")
lapply(x, require, character.only = TRUE)

#--------dive plot prep-------------------
forage_dives <- read.csv("5.1 forage dives pqi w bout 210212.csv")
no_incstart <- subset(forage_dives,forage_dives$stage!="inc at start")
no_incstart$stage <- factor(no_incstart$stage,levels = c("incubation", "guard"))

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#-----------diving depth of foraging dives by stage--------
no_incstart$stage <- gsub("guard", "brood", no_incstart$stage)
  
dive_plot <- ggplot(no_incstart, aes(x=year, y=max_depth,fill=stage)) +
  geom_boxplot()+theme_bw()+labs(fill="")+
  ylab("foraging dive depth (m)")+xlab("colony")+
  scale_fill_manual(limits=c("incubation","brood"),
                     values=c(cbPalette[6],cbPalette[4]))+
  theme(text = element_text(size=14),axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),legend.text=element_text(size=14))+  
  scale_y_continuous(trans = "reverse")

