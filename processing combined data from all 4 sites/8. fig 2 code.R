x<-c("ggplot2","plyr","raster","rgeos","rgdal",
     "maptools","ggsn")
lapply(x, require, character.only = TRUE)

#add coastline
coast = readOGR("D:/mac desktop Dec12 2018/penguin analysis/Coastline_high_res_polygon", 
                "Coastline_high_res_polygon",verbose = TRUE)
prj <- proj4string(coast)
coast<-spTransform(coast, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")) 

setwd("D:/Published papers/Chinstrap chapter April28 2020/analysis/3 chinstrap code Sept27 2019")
all_GPS <- read.csv("4.gps interp 1min 1h removed 210212.csv")


data <- all_GPS
  data$pts <- data[c("interp_Lon","interp_Lat")] 
  coordinates(data$pts) <- ~interp_Lon+interp_Lat   
  proj4string(data$pts) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

  latlimits <- c(-60.85,-60.45)
  longlimits <- c(-46.2,-44.35)
  
  ## Clip the coastline shapefile
  coast2 <- crop(coast, extent(longlimits[1],longlimits[2],latlimits[1],latlimits[2]))
  
  ### Fortify spatial polygons dataframe into regular df format for use in ggplot2
  coast2@data$id = rownames(coast2@data)
  coast2.points = fortify(coast2, region="id")
  coast2.df = join(coast2.points, coast2@data, by="id")   
  
my_plot2 <- ggplot() + 
    geom_polygon(data=coast2.df, aes(x = long, y = lat, group = id, fill=surface)) +
    #makes land black and sea white 
    scale_fill_manual(values=c("land"="black","NA"="white"))+
    geom_point(data=data[which(data$year == "Laurie"),],
               aes(x=pts$interp_Lon,y=pts$interp_Lat),col='chartreuse2',size=0.5)+
    geom_point(data=data[which(data$year == "Powell"),],
               aes(x=pts$interp_Lon,y=pts$interp_Lat),col='purple',size=0.5)+
    geom_point(data=data[which(data$year == "Monroe"),],
               aes(x=pts$interp_Lon,y=pts$interp_Lat),col='maroon1',size=0.5)+
    geom_point(data=data[which(data$year == "Signy"),],
               aes(x=pts$interp_Lon,y=pts$interp_Lat),col='dodgerblue3',size=0.5)+
    
    theme_bw() + coord_fixed(ratio=1,expand=FALSE) +
    xlab("\nLongitude")+ylab("Latitude\n")+ 
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = 'none',
          legend.direction= 'vertical',
          panel.border	= element_rect(colour='white'),
          axis.text		= element_text(size=11),
          axis.title		= element_text(size=13))
  