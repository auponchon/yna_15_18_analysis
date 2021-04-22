load("Data/NewlyCreatedData/loc_interpolated_states_day_2015.RData")
load("Data/NewlyCreatedData/loc_interpolated_states_day_2018.RData")

source("R scripts/functions.R")

suc2015<-subset(loc.interpolated.day.2015,loc.interpolated.day.2015$Status=="success",drop=T)
fail2015<-subset(loc.interpolated.day.2015,loc.interpolated.day.2015$Status=="fail",drop=T)
suc2018<-subset(loc.interpolated.day.2018,loc.interpolated.day.2018$Status=="success",drop=T)
fail2018<-subset(loc.interpolated.day.2018,loc.interpolated.day.2018$Status=="fail",drop=T)

## Packages ? charger
library(maps)
library(trip)
library(adehabitatHR)
library(viridis)

# r?solution de la grille
res=.05
ylim_min=-46
ylim_max=-30
xlim_min=62
xlim_max=95
#
Sumsuc2015<-get_saturation(suc2015,res,ylim_min, ylim_max, xlim_min, xlim_max)
save(Sumsuc2015,file="Data/NewlyCreatedData/Representativeness_suc2015_InitMethod.RData")    

Sumsuc2018<-get_saturation(suc2018,res,ylim_min, ylim_max, xlim_min, xlim_max)
save(Sumsuc2018,file="Data/NewlyCreatedData/Representativeness_suc2018_InitMethod.RData") 

Sumfail2015<-get_saturation(fail2015,res,ylim_min, ylim_max, xlim_min, xlim_max)
save(Sumfail2015,file="Data/NewlyCreatedData/Representativeness_fail2015_InitMethod.RData")    

Sumfail2018<-get_saturation(fail2018,res,ylim_min, ylim_max, xlim_min, xlim_max)
save(Sumfail2018,file="Data/NewlyCreatedData/Representativeness_fail2018_InitMethod.RData") 


sat<-rbind(Sumfail2015)


load("Saturation plots/Representativeness_suc2015_InitMethod.RData")
load("Saturation plots/Representativeness_suc2018_InitMethod.RData")
load("Saturation plots/Representativeness_fail2015_InitMethod.RData")
load("Saturation plots/Representativeness_fail2018_InitMethod.RData")

colo<-inferno(2,begin=0.35,end=0.8)


tiff("Figures/Saturation_plot_old.tif",height=2500,width=4000,res=400,compression="lzw")
#load("Saturation plots/Representativeness_suc2015_InitMethod.RData")
par(mar=c(2.2,1.8,1,1))
plot(c(0,Sumsuc2015$rank1), c(0,Sumsuc2015$nb), type="n",xlim=c(0,15),ylim=c(0,6000),
     cex.axis=1.2 , bty="n" ,xaxt="n",yaxt="n" )

axis(1,at=seq(0,15,3),cex.axis=1.2,cex.lab=1.2,pos=0)
axis(2,at=seq(0,6000,1500),cex.axis=1.2,cex.lab=1.2,pos=0)

#load("Saturation plots/Representativeness_suc2018_InitMethod.RData")
lines(c(0,Sumsuc2018$rank1), c(0,Sumsuc2018$nb),  col=colo[1],lwd=4,lty=2)                                            
points(c(0,Sumsuc2018$rank1), c(0,Sumsuc2018$nb), pch=22, bg=colo[1],cex=1.2)

#load("Saturation plots/Representativeness_fail2015_InitMethod.RData")
lines(c(0,Sumfail2018$rank1), c(0,Sumfail2018$nb), col=colo[2],lwd=4,lty=2)
points(c(0,Sumfail2018$rank1), c(0,Sumfail2018$nb), pch=25, bg=colo[2],cex=1.4)

#load("Saturation plots/Representativeness_fail2015_InitMethod.RData")
lines(c(0,Sumfail2015$rank1), c(0,Sumfail2015$nb), col=colo[2],lwd=4)
points(c(0,Sumfail2015$rank1), c(0,Sumfail2015$nb), pch=24, bg=colo[2],cex=1.4)


lines(c(0,Sumsuc2015$rank1), c(0,Sumsuc2015$nb),  col=colo[1],lwd=4)
points(c(0,Sumsuc2015$rank1), c(0,Sumsuc2015$nb), pch=21, bg=colo[1],cex=1.4)



legend(10,1500,legend=c("Success 2015","Fail 2015","Success 2018","Fail 2018"),pch=c(21,24,22,25),
      lty=c(1,1,2,2), box.lty=1.5 ,lwd=2, pt.bg=c(colo[1],colo[2],colo[1],colo[2]),
      pt.cex=c(1.2),cex=1.4,y.intersp=0.8,
      col=c(colo[1],colo[2],colo[1],colo[2]))
mtext(1,line=1,text="Number of tracked individuals",cex=1.3)
mtext(2,line=0.5,text="Area of active use (km²)",cex=1.3)

 dev.off()