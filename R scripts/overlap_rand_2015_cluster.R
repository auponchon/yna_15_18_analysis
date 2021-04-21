library(DescTools)
library(rgdal)
library(maptools)
library(adehabitatHR)

load("loc_interpolated_states_day_2015.RData")

r.cor.true<-NULL

loc.2015<-subset(loc.interpolated.day.2015,loc.interpolated.day.2015$States=="1" |
                    loc.interpolated.day.2015$States=="3")

loc.2015<-droplevels(loc.2015)

faily<-unique(loc.2015$ID[which(loc.2015$Status=="fail")])
sucy<-unique(loc.2015$ID[which(loc.2015$Status=="success")])
id<-unique(loc.2015$ID)

loc.obs<-loc.2015
coordinates(loc.obs)<-c("Longitude","Latitude")
proj4string(loc.obs) <- CRS("+proj=longlat +ellps=WGS84")
loc<-spTransform(loc.obs, CRS("+proj=utm +zone=45 +ellps=WGS84"))

kov.50<-  kerneloverlap(loc.obs[,1],method="UDOI",grid=1000,percent=50)
kov.90<-  kerneloverlap(loc.obs[,1],method="UDOI",grid=1000,percent=90)
diag(kov.50)<-NA
diag(kov.90)<-NA
mat.bino<-kov.50

for (i in 1:nrow(mat.bino)){
    for (j in 1:ncol(mat.bino)){
        ifelse(rownames(mat.bino)[i] == colnames(mat.bino)[j],mat.bino[i,j]<-NA,
               ifelse(rownames(mat.bino)[i]  %in% faily & 
                          colnames(mat.bino)[j] %in% faily,
                      mat.bino[i,j]<-0,
                      ifelse(rownames(mat.bino)[i]  %in% sucy & 
                                 colnames(mat.bino)[j] %in% sucy,
                             mat.bino[i,j]<-0,
                             mat.bino[i,j]<- 1)))
    }
}

r.cor.50.true<-cor(as.vector(kov.50), as.vector(mat.bino),method="pearson",use="pairwise.complete.obs")
r.cor.90.true<-cor(as.vector(kov.90), as.vector(mat.bino),method="pearson",use="pairwise.complete.obs")

#####################################################################################
####RANDOMIZED CORRELATIONS

r.cor.rand<-NULL

for ( p in 1:1000){
    new.loc<-NULL
    print(p)
    
    for ( n in 1:length(id)){
        loc.rand<-subset(loc.2015, loc.2015$ID==id[n])
        loc.rand<-droplevels(loc.rand)
        angl<-runif(1,0,2*pi)
    
        if(nrow(loc.rand)>=6){
            
            loc.rot<-Rotate(x=loc.rand$Longitude, y = loc.rand$Latitude, theta = angl, asp = 1)
            loc.rot<-data.frame(ID=as.factor(rep(id[n],times=length(loc.rot$x))),
                                Longitude=loc.rot$x,Latitude=loc.rot$y)
            new.loc<-rbind(new.loc,loc.rot)
        }
    }
    
    new.loc<-droplevels(new.loc)
    coordinates(new.loc)<-c("Longitude","Latitude")
    proj4string(new.loc) <- CRS("+proj=longlat +ellps=WGS84")
    new.loc<-spTransform(new.loc, CRS("+proj=utm +zone=45 +ellps=WGS84"))
    
    kov.50<-  kerneloverlap(new.loc[,1],method="UDOI",grid=500,percent=50)
    kov.90<-  kerneloverlap(new.loc[,1],method="UDOI",grid=500,percent=90)
    diag(kov.50)<-NA
    diag(kov.90)<-NA
    mat.bino<-kov.50
    
    for (i in 1:nrow(mat.bino)){
        for (j in 1:ncol(mat.bino)){
            ifelse(rownames(mat.bino)[i] == colnames(mat.bino)[j],
                   mat.bino[i,j]<-NA,
                   ifelse(rownames(mat.bino)[i]  %in% faily & colnames(mat.bino)[j] %in% faily, 
                          mat.bino[i,j]<-0,
                          ifelse(rownames(mat.bino)[i]  %in% sucy & colnames(mat.bino)[j] %in% sucy,
                                 mat.bino[i,j]<-0,
                                 mat.bino[i,j]<- 1)))
        }
    }
    
    
    r.cor.50<-cor(as.vector(kov.50), as.vector(mat.bino),method="pearson",use="pairwise.complete.obs")
    r.cor.90<-cor(as.vector(kov.90), as.vector(mat.bino),method="pearson",use="pairwise.complete.obs")
    
    r.cor.rand<-rbind(r.cor.rand,data.frame(Iter=p,HR=50,R.cor=r.cor.50))
    r.cor.rand<-rbind(r.cor.rand,data.frame(Iter=p,HR=90,R.cor=r.cor.90))
    
} 

write.table(r.cor.rand,file="r_cor_rand_fail_suc_2015.txt",row.names=F,quote=F,sep="\t")

