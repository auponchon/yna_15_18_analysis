######################################################################################
## Function to give a trip number based on the consecutive distances to the colony
######################################################################################

define_trips<-function(dataset){

id<-unique(dataset$ID)
ALL<-NULL

for (i in 1:length(id)){
    indi<-subset(dataset,dataset$ID==id[i])
    indi$Difftime<-c(0,difftime(indi$DateTime[2:nrow(indi)],
                                     indi$DateTime[1:nrow(indi)-1],units="mins"))
    
    w<-1
    for (a in 1:nrow(indi)){
        #Define points away from the colony  
        ifelse(indi[a,"Distmax"]>=dist.thres,  
               indi[a,"TravelNb"]<-w,
               indi[a,"TravelNb"]<-0)  # if distance is below, the bird is considered in the colony and TravelNb = 0
        ifelse(indi[a,"TravelNb"]==0,w<-w+1,w<-w)
    } 
    
    trip.nb<-sort(unique(indi$TravelNb))
    v<-1
    
    #assign consecutive trip numbers
    if(length(trip.nb)>1){
        for (z in 2:length(trip.nb)){
            
            trip<-which(indi$TravelNb==trip.nb[z])
            ifelse(indi$DateTime[trip[length(trip)]] != indi$DateTime[nrow(indi)],
                   number<-c(trip[1]-1,trip,trip[length(trip)]+1),
                   number<-c(trip[1]-1,trip))
            if(length(number)>row.thres & sum(indi$Difftime[number]) > dur.thres){
                indi$TravelNb[number]<-v
                v<-v+1
            }   #if trip ok, consecutive number
            else{indi$TravelNb[number]<-0}
        } #end of loop within trips
        
        #calculate total distance for the trip
        for (y in 2:nrow(indi)){
            if(indi$TravelNb[y]>0){
                indi[y,"PathLength"]<-indi$Distadj[y]+indi$PathLength[y-1]}
        }
    }
    
    ALL<-rbind(ALL,indi)
}
return(ALL)
}

######################################################################################
## Function to give the summary of all raw trips by individuals
######################################################################################
    
raw_trips_summary_ind<-function (dataset){
        id<-unique(dataset$ID) 
        dist.max.all.trips<-NULL
        
        for (i in 1:length(id)){
            temp<-subset(dataset,dataset$ID==id[i])
            nb.trip<-sort(unique(temp$TravelNb))
            
            if (length(nb.trip)>1){
                for (a in 2:length(nb.trip)){
                    tempo<-subset(temp,temp$TravelNb==nb.trip[a])
                    
                    maxi<-data.frame(ID=id[i],Status=tempo$Status[1],TravelNb=nb.trip[a],
                                     Distmax=max(tempo$Distmax),TripDur=sum(tempo$Difftime)/60,
                                     maxDiffTime=max(tempo$Difftime)/60,TotalPath=tempo$PathLength[nrow(tempo)])
                    dist.max.all.trips<-rbind(dist.max.all.trips,maxi)
                    
                }
            }
        }
        

    return(dist.max.all.trips)
}

######################################################################################
## Function to give the summary of all raw trips by status
######################################################################################

raw_trips_summary_status<-function (dataset){
    id<-unique(dataset$ID) 
    dist.max.all.trips<-NULL
    
    for (i in 1:length(id)){
        temp<-subset(dataset,dataset$ID==id[i])
        nb.trip<-sort(unique(temp$TravelNb))
        
        if (length(nb.trip)>1){
            for (a in 2:length(nb.trip)){
                tempo<-subset(temp,temp$TravelNb==nb.trip[a])
                
                maxi<-data.frame(ID=id[i],Status=tempo$Status[1],TravelNb=nb.trip[a],
                                 Distmax=max(tempo$Distmax),TripDur=sum(tempo$Difftime)/60,
                                 maxDiffTime=max(tempo$Difftime)/60,TotalPath=tempo$PathLength[nrow(tempo)])
                dist.max.all.trips<-rbind(dist.max.all.trips,maxi)
                
            }
        }
    }
    
    dist.max.all.trips<-      ddply(dist.max.all.trips,.(Status),summarize,
                                    NbInd=length(unique(ID)),
                                    NbTravel=length(TravelNb),
                                    MeanDist=mean(Distmax), SEDist= std.error(Distmax, na.rm=T),
                                    MinDist=min(Distmax),MaxDist=max(Distmax),
                                    MeanDur=mean(TripDur),SEDur= std.error(TripDur, na.rm=T),
                                    MinTripDur=min(TripDur),MaxTripDur=max(TripDur),
                                    MeanTotPath=mean(TotalPath),SETotPath=std.error(TotalPath, na.rm=T),
                                    MinTotPath=min(TotalPath),MaxTotPath=max(TotalPath))
    return(dist.max.all.trips)
}

######################################################################################
## Function to clean trips based on trip thresholds and give summary by individuals
######################################################################################

clean_trips_summary_ind<-function (dataset,diff.threshold){
    summary.clean.trips<-NULL
    id<-unique(dataset$ID)
    
for (i in 1:length(id)){
    temp<-subset(dataset,dataset$ID==id[i])
    nb.trip<-sort(unique(temp$TravelNb))
    
    if (length(nb.trip)>1){
        for (a in 2:length(nb.trip)){
            tempo<-subset(temp,temp$TravelNb==nb.trip[a])
       
        if(tempo$Distmax[nrow(tempo)] < last.dist){
                if (tempo$DateTime[nrow(tempo)] < date.max){
                    if (max(tempo$Difftime) <= diff.thres){
                        
                        dada<- data.frame(ID=id[i],Status=tempo$Status[1],TravelNb=nb.trip[a],
                                          Distmax=max(tempo$Distmax),TripDur=sum(tempo$Difftime)/60,
                                          maxDiffTime=max(tempo$Difftime)/60,TotalPath=tempo$PathLength[nrow(tempo)])
                        
                        summary.clean.trips<-rbind(summary.clean.trips,dada)
                        
                    }  #end of if difftime 
                }  #end of if datetime ok
              }   #end of if last line is on the nest
        } #end of trip loop within individuals
    } #end of if nb of trips > 0
} #end of loop on individuals
    

    return(summary.clean.trips)
    
} #end of function

######################################################################################
## Function to clean trips based on trip thresholds and give summary by individuals
######################################################################################

clean_trips_summary_status<-function (dataset,diff.threshold){
    summary.clean.trips<-NULL
    id<-unique(dataset$ID)
    
    for (i in 1:length(id)){
        temp<-subset(dataset,dataset$ID==id[i])
        nb.trip<-sort(unique(temp$TravelNb))
        
        if (length(nb.trip)>1){
            for (a in 2:length(nb.trip)){
                tempo<-subset(temp,temp$TravelNb==nb.trip[a])
                
                if(tempo$Distmax[nrow(tempo)] < last.dist){
                    if (tempo$DateTime[nrow(tempo)] < date.max){
                        if (max(tempo$Difftime) <= diff.thres){
                            
                            dada<- data.frame(ID=id[i],Status=tempo$Status[1],TravelNb=nb.trip[a],
                                              Distmax=max(tempo$Distmax),TripDur=sum(tempo$Difftime)/60,
                                              maxDiffTime=max(tempo$Difftime)/60,TotalPath=tempo$PathLength[nrow(tempo)])
                            
                            summary.clean.trips<-rbind(summary.clean.trips,dada)
                            
                        }  #end of if difftime 
                    }  #end of if datetime ok
                }   #end of if last line is on the nest
            } #end of trip loop within individuals
        } #end of if nb of trips > 0
    } #end of loop on individuals
    
    
    summary.clean.trips<-    ddply(summary.clean.trips,.(Status),summarize,
                                   NbInd=length(unique(ID)),
                                   NbTravel=length(TravelNb),
                                   MeanDist=mean(Distmax), SEDist= std.error(Distmax, na.rm=T),
                                   MinDist=min(Distmax),MaxDist=max(Distmax),
                                   MeanDur=mean(TripDur),SEDur= std.error(TripDur, na.rm=T),
                                   MinTripDur=min(TripDur),MaxTripDur=max(TripDur),
                                   MeanTotPath=mean(TotalPath),SEtotPath=std.error(TotalPath, na.rm=T),
                                   MinTotPath=min(TotalPath),MaxTotPath=max(TotalPath))
    return(summary.clean.trips)
    
} #end of function

######################################################################################
## Function to clean trips based on trip thresholds and give locations
######################################################################################


clean_trips_locations<-function (dataset,diff.threshold){
    clean.trips.loc<-NULL
    id<-unique(dataset$ID)
    
    for (i in 1:length(id)){
        temp<-subset(dataset,dataset$ID==id[i])
        nb.trip<-sort(unique(temp$TravelNb))
        
        if (length(nb.trip)>1){
            for (a in 2:length(nb.trip)){
                tempo<-subset(temp,temp$TravelNb==nb.trip[a])
                
                if(tempo$Distmax[nrow(tempo)] < last.dist){
                    if (tempo$DateTime[nrow(tempo)] < date.max){
                        if (max(tempo$Difftime) <= diff.thres){
                            clean.trips.loc<-rbind(clean.trips.loc,tempo)
                            
                        }  #end of if difftime 
                    }  #end of if datetime ok
                }   #end of if last line is on the nest
            } #end of trip loop within individuals
        } #end of if nb of trips > 0
    } #end of loop on individuals
    
    return(clean.trips.loc)
    
} #end of function


######################################################################################
## Function to print a legend in ggplot when using grid.arrange())
######################################################################################

gg_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}


######################################################################################
## Function to perform a saturation plot to test for representativit
######################################################################################

get_saturation<-function(DATA,res,ymin,ymax,xmin,xmax){
    gt <- GridTopology(cellcentre.offset=c(xlim_min,ylim_min), cellsize = c(res, res),
                       cells.dim=c((xlim_max-xlim_min)/res,(ylim_max-ylim_min)/res))
    
    Liste <- unique(as.factor(DATA$ID))
    i=1
    NB=NULL # Initialisation
    #

for (i in 1:length(Liste)){
    print(Liste[i])
    
    #    X <- read.csv(paste("D:/Bureau/Travail/CSV_PortCros/", Liste[i], sep=""), header=T, sep=",")
    X <- DATA[DATA$ID==Liste[i],]
    ##    X$Date <- as.character(X$Date)
    ##    X$Hour <- as.character(X$Time)
    ##    X$the_date <- paste(X$Date, X$Hour, sep=" ")
    ##    X$the_date <- as.POSIXct(strptime(X$the_date , format="%Y/%m/%d %H:%M:%S", tz="GMT"))
    ##
    ##X$ID <- substr(Liste[i],1,nchar(Liste[i])-4)
    
    X$ID2 <- i
    X$ID2 <- as.factor(X$ID2)
    X$Num<- seq(1,nrow(X),1)
    #    # suppression des duplicas
    X <- X[duplicated(X$DateTime)==FALSE,]
    #
    #       # 1) temps pass? par secteur :
    trajet <- data.frame(id=X$ID2,long=X$Longitude,lat=X$Latitude,date=X$DateTime)
    trajet2 <- trajet[order(trajet$id, trajet$date),]
    
    #        		# Conversion en classe 'trip'
    coordinates(trajet) <- ~long + lat
    trajett <- trip(trajet, c("date", "id"))
    proj4string(trajett) <- CRS("+proj=longlat +ellps=WGS84") # def du systeme de coord
    
    #            # Create a grid of time spent by approximating the time between locations for separate trip events.
    trg <- tripGrid.interp(trajett, grid = gt, method="count", dur = 600)   #   d?coupage, 1 point = 300 sec
    #
    IndexTrg <- getGridIndex(coordinates(trg), gt, all.inside = TRUE)
    #
    trg1 <- data.frame(trg)
    trg1$bloc <- IndexTrg
    trg1$s1 <- round(trg1$s1,3)
    trg1$s2 <- round(trg1$s2,3)
    trg1$centroid.lon <- round(round(trg1$s1,3) + 0.1/2 ,2)
    trg1$centroid.lat <- round(round(trg1$s2,3) + 0.1/2 ,2)
    trg1$ID <- i
    trg1 <- subset(trg1,trg1$z !=0)
    
    nb <- data.frame(id=i, num.cell=trg1$bloc)
    NB <- rbind(NB, nb)
}
NB$id <- as.factor(NB$id)

seq <- rev(order(table(NB$id)))
SEQ <- data.frame(rank=seq(1,length(seq),1), id=seq)
NB$id <- as.factor(NB$id)
str(NB)
tot = NULL
for (i in 1:length(seq)){
    sub <- NB[NB$id==i,]
    sub$rank1 <- SEQ$rank[SEQ$id==i]
    tot <- rbind(tot, sub)
}

tot <- tot[order(tot$rank1),]

B <- tot[tot$rank1==1,]
Sum <- data.frame(id=B$id[1], rank1=1, nb=nrow(B))

for (i in 2: length(seq)){
    print(i)
    Index.B <- unique(B$num.cell)
    Xp <- tot[tot$rank1==i,]
    Xp.unique <- Xp[!Xp$num.cell%in%Index.B,]
    B <- rbind(B, Xp)
    Sum <- rbind(Sum, data.frame(id=Xp$id[i], rank1= Xp$rank1[i], nb=length(Index.B)+nrow(Xp.unique)))
}
return(as.data.frame(Sum))

}

