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
            nb.trip<-unique(temp$TravelNb)
            
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
        nb.trip<-unique(temp$TravelNb)
        
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
                                    MeanDist=mean(Distmax), SDDist= std.error(Distmax, na.rm=T),
                                    MinDist=min(Distmax),MaxDist=max(Distmax),
                                    MeanDur=mean(TripDur),SDDur= std.error(TripDur, na.rm=T),
                                    MinTripDur=min(TripDur),MaxTripDur=max(TripDur),
                                    MinmaxDiff=min(maxDiffTime),MaxmaxDiff=max(maxDiffTime),
                                    MeanTotPath=mean(TotalPath),SDDur=std.error(TotalPath, na.rm=T),
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
    nb.trip<-unique(temp$TravelNb)
    
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
        nb.trip<-unique(temp$TravelNb)
        
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
                                   MeanDist=mean(Distmax), SDDist= std.error(Distmax, na.rm=T),
                                   MinDist=min(Distmax),MaxDist=max(Distmax),
                                   MeanDur=mean(TripDur),SDDur= std.error(TripDur, na.rm=T),
                                   MinTripDur=min(TripDur),MaxTripDur=max(TripDur),
                                   MinmaxDiff=min(maxDiffTime),MaxmaxDiff=max(maxDiffTime),
                                   MeanTotPath=mean(TotalPath),SDDur=std.error(TotalPath, na.rm=T),
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
        nb.trip<-unique(temp$TravelNb)
        
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



