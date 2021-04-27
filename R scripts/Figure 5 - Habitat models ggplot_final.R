library(ggplot2)
library(raster)
library(rasterVis)
library(viridis)
library(rgdal)
library(dplyr)
library(ggspatial)
library(sf)
library(ggsn)


suc.2015<-raster("Maxent modelling/Maxent 2015/OutputSuccess/success_avg.asc")
fail.2015<-raster("Maxent modelling/Maxent 2015/OutputFail/fail_avg.asc")
suc.2018<-raster("Maxent modelling/Maxent 2018/OutputSuccess/success_avg.asc")
fail.2018<-raster("Maxent modelling/Maxent 2018/OutputFail/fail_avg.asc")

colony<-data.frame(Longitude =77.52356,Latitude=-37.8545)
sfPoints <- st_as_sf(colony, coords = c("Longitude", "Latitude"), crs = 4326)


mapyx<-readOGR(dsn="D:/Aurore/Boulot/ArcGIS/Cartes",layer="World map")

exto<-extent(fail.2015)

mapyx<-crop(mapyx,exto)
#mapyx@data$id <- rownames(mapyx@data)
mapyx.f <- fortify(mapyx)
#mapyx_df <- left_join(mapyx.f, mapyx@data, by = "id")




suc.2018<-crop(suc.2018,exto)
fail.2018<-crop(fail.2018,exto)

rast<-stack(suc.2015,fail.2015,suc.2018,fail.2018)

# fish<-read.csv("C:/Users/s03ap7/Dropbox/Aurora_CellCatch_Indian_Southern_Ocean.csv",header=T)
# fish.rast<-rasterFromXYZ(fish[,c(2,3,12)],res=0.5)
# fish.rast<-crop(fish.rast,exto)
# fish.rast[fish.rast<0.001]<-NA

ras.dat<-as.data.frame(rast,xy=T)

col<-c("blue","cyan","green","yellow","orange","red","darkred")
brk<-c(0,0.2,0.40,0.60,0.80,1)


# rtp <- rasterToPolygons(fish.rast)
# rtp@data$id <- 1:nrow(rtp@data)   # add id column for join
# rtpFort <- fortify(rtp, data = rtp@data)
# rtpFortFish <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
# 


map<-ggplot(ras.dat,aes(x=x,y=y)) +
  geom_raster( aes(fill=success_avg.1),show.legend=F) +
#  geom_raster(data = rtpFortFish, aes(x = long, y = lat, alpha = log(catch_sum))) +
  scale_x_continuous(breaks=seq(60,100,10),expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-50,-30,5),expand=c(0,0)) +
  annotation_north_arrow(location = "tl", height = unit(1, "cm"), width = unit(0.6, "cm"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_orienteering) +
  geom_polygon(data =fortify(mapyx),
               aes(x=long,y=lat,group=group),
              color="grey30", fill="grey",size=0.35) + 
  scalebar(dist = 250, dist_unit="km", transform=T,st.dist=0.06,st.size=4,
           height=0.03, model = 'WGS84',x.min=60,y.min=-48.5, x.max=97,y.max=-35) +
  scale_fill_gradientn(  breaks=brk,name="",
                        limits=c(0,1),colours=col) +
  geom_point(data = colony, aes(x=Longitude,y=Latitude),size = 6,shape=17) +
  ylab("Latitude (°C)") +
  xlab("")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  ggtitle("a) Success 2015")# +

# mapp<-map + geom_polygon(data = rtpFortFish, 
#                          aes(x = long, y = lat, group = group, fill = catch_sum), 
#                          alpha = 0.7, 
#                          size = 0)# +  ## size = 0 to remove the polygon outlines
#  coord_quickmap()

print(map)

map2<-ggplot(ras.dat,aes(x=x,y=y)) +
  geom_raster( aes(fill=fail_avg.1),show.legend=F) +
#  geom_raster(data = rtpFortFish, aes(x = long, y = lat, alpha = log(catch_sum))) +
  scale_x_continuous(breaks=seq(60,100,10),expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-50,-30,5),expand=c(0,0)) +
  annotation_north_arrow(location = "tl", height = unit(1, "cm"), width = unit(0.6, "cm"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_orienteering) +
  geom_polygon(data =fortify(mapyx),
               aes(x=long,y=lat,group=group),
              color="grey30", fill="grey",size=0.35) + 
  scalebar(dist = 250, dist_unit="km", transform=T,st.dist=0.06,st.size=4,
           height=0.03, model = 'WGS84',x.min=60,y.min=-48.5, x.max=97,y.max=-35) +
  # scale_fill_viridis(option="magma",name="") +
  scale_fill_gradientn(  breaks=brk,name="",
                         limits=c(0,1),colours=col) +
  geom_point(data = colony, aes(x=Longitude,y=Latitude),size = 6,shape=17) +
  #  geom_text(label="???", size=10, family = "HiraKakuPro-W3") +
  ylab("") +
  xlab("")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  ggtitle("b) Fail 2015")# 

# map22<-map2 + geom_polygon(data = rtpFortFish, 
#                          aes(x = long, y = lat, group = group, fill = catch_sum), 
#                          alpha = 0.7, 
#                          size = 0)# +  ## size = 0 to remove the polygon outlines
#
print(map2)

map3<-ggplot(ras.dat,aes(x=x,y=y)) +
  geom_raster( aes(fill=success_avg.2),show.legend = F) +
#  geom_raster(data = rtpFortFish, aes(x = long, y = lat, alpha = log(catch_sum))) +
  scale_x_continuous(breaks=seq(60,100,10),expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-50,-30,5),expand=c(0,0)) +
  annotation_north_arrow(location = "tl", height = unit(1, "cm"), width = unit(0.6, "cm"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_orienteering) +
  
  geom_polygon(data =fortify(mapyx),
               aes(x=long,y=lat,group=group),
               color="grey30", fill="grey",size=0.35) + 
  scalebar(dist = 250, dist_unit="km", transform=T,st.dist=0.06,st.size=4,
           height=0.03, model = 'WGS84',x.min=60,y.min=-48.5, x.max=97,y.max=-35) +
  # scale_fill_viridis(option="magma",name="") +
  scale_fill_gradientn(  breaks=brk,name="",
                         limits=c(0,0.9),colours=col) +
  geom_point(data = colony, aes(x=Longitude,y=Latitude),size = 6,shape=17) +
  #  geom_text(label="???", size=10, family = "HiraKakuPro-W3") +
  ylab("Latitude (°C)") +
  xlab("Longitude (°C)")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  ggtitle("c) Success 2018")# +
print(map3)

map4<-ggplot(ras.dat,aes(x=x,y=y)) +
  geom_raster( aes(fill=fail_avg.2)) +
#  geom_raster(data = rtpFortFish, aes(x = long, y = lat, alpha = log(catch_sum))) +
  scale_x_continuous(breaks=seq(60,100,10),expand=c(0,0)) +
  scale_y_continuous(breaks=seq(-50,-30,5),expand=c(0,0)) +
  annotation_north_arrow(location = "tl", height = unit(1, "cm"), width = unit(0.6, "cm"),
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),
                         style = north_arrow_orienteering) +
  
  geom_polygon(data =fortify(mapyx),
               aes(x=long,y=lat,group=group),
               color="grey30", fill="grey",size=0.35) + 
  scalebar(dist = 250, dist_unit="km", transform=T,st.dist=0.06,st.size=4,
           height=0.03, model = 'WGS84',x.min=60,y.min=-48.5, x.max=97,y.max=-35) +
  # scale_fill_viridis(option="magma",name="") +
  scale_fill_gradientn(  breaks=brk,name="Presence \nprobability",
                         limits=c(0,0.9),colours=col) +
  geom_point(data = colony, aes(x=Longitude,y=Latitude),size = 6,shape=17) +
  #  geom_text(label="???", size=10, family = "HiraKakuPro-W3") +
  ylab("") +
  xlab("Longitude (°C)")+
  theme_bw() +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12),
        legend.text = element_text(size=12))+
  ggtitle("d) Fail 2018")# +
print(map4)

library(gridExtra)
library(grid)

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(map4)


tiff("Figures/Figure 5 - Presence_proba.tiff",height=3000,width=5000,res=400,
     compression="lzw")
grid.arrange(arrangeGrob(arrangeGrob(map+ theme(legend.position="none") ) ,
                         arrangeGrob(map2+ theme(legend.position="none")) ,
                         arrangeGrob(map3+ theme(legend.position="none")) ,
                         arrangeGrob(map4+ theme(legend.position="none")) ,
                         ncol=2,nrow=2),
             mylegend, ncol=2,widths=c(1,0.1))

dev.off()

#levelplot(rast)



