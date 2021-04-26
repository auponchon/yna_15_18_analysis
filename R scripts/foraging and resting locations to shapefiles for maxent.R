load("Data/NewlyCreatedData/loc_interpolated_states_day_2015.RData")
load("Data/NewlyCreatedData/loc_interpolated_states_day_2018.RData")
library(sf)
library(rgdal)

loc.for.2015<-subset(loc.interpolated.day.2015,loc.interpolated.day.2015$States=="1"|
                         loc.interpolated.day.2015$States=="3")

loc.for.2018<-subset(loc.interpolated.day.2018,loc.interpolated.day.2018$States=="1"|
                         loc.interpolated.day.2018$States=="3")

loc.2015.fail<-subset(loc.for.2015,loc.for.2015$Status=="fail")

loc.2015.fail$x<-loc.2015.fail$Longitude
loc.2015.fail$y<-loc.2015.fail$Latitude

coordinates(loc.2015.fail)<-c("Longitude","Latitude")
proj4string(loc.2015.fail) <- CRS("+proj=longlat +ellps=WGS84")

writeOGR(loc.2015.fail, dsn="Maxent modelling/Maxent 2015/Occurence/RarefyOccurence",
         layer="loc_for_fail_2015", driver="ESRI Shapefile",overwrite_layer=T)


loc.2015.suc<-subset(loc.for.2015,loc.for.2015$Status=="success")

loc.2015.suc$x<-loc.2015.suc$Longitude
loc.2015.suc$y<-loc.2015.suc$Latitude

coordinates(loc.2015.suc)<-c("Longitude","Latitude")
proj4string(loc.2015.suc) <- CRS("+proj=longlat +ellps=WGS84")

writeOGR(loc.2015.suc, dsn="Maxent modelling/Maxent 2015/Occurence/RarefyOccurence",
         layer="loc_for_suc_2015", driver="ESRI Shapefile",overwrite_layer=T)

loc.2018.fail<-subset(loc.for.2018,loc.for.2018$Status=="fail")

loc.2018.fail$x<-loc.2018.fail$Longitude
loc.2018.fail$y<-loc.2018.fail$Latitude

coordinates(loc.2018.fail)<-c("Longitude","Latitude")
proj4string(loc.2018.fail) <- CRS("+proj=longlat +ellps=WGS84")

writeOGR(loc.2018.fail, dsn="Maxent modelling/Maxent 2018/Occurence/RarefyOccurence",
         layer="loc_for_fail_2018", driver="ESRI Shapefile",overwrite_layer=T)

loc.2018.suc<-subset(loc.for.2018,loc.for.2018$Status=="success")

loc.2018.suc$x<-loc.2018.suc$Longitude
loc.2018.suc$y<-loc.2018.suc$Latitude

coordinates(loc.2018.suc)<-c("Longitude","Latitude")
proj4string(loc.2018.suc) <- CRS("+proj=longlat +ellps=WGS84")

writeOGR(loc.2018.suc, dsn="Maxent modelling/Maxent 2018/Occurence/RarefyOccurence",
         layer="loc_for_suc_2018", driver="ESRI Shapefile",overwrite_layer=T)
