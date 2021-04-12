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
return(ALL.)

}
