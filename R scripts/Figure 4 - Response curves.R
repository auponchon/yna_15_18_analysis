library(stringr)
library(plyr)

path.fail.15<-"Maxent modelling/Maxent 2015/OutputFail/plots/FINAL/"
path.succes.15<-"Maxent modelling/Maxent 2015/OutputSuccess/plots/FINAL/"
path.fail.18<-"Maxent modelling/Maxent 2018/OutputFail/plots/FINAL/"
path.success.18<-"Maxent modelling/Maxent 2018/OutputSuccess/plots/FINAL/"



fil<-list.files(path=path.fail.15,pattern="_only.dat")
nam<-str_replace_all(fil,pattern="_only.dat","_2015")

fila<-list.files(path=path.succes.15,pattern="_only.dat")
nama<-str_replace_all(fila,pattern="_only.dat","_2015")


filo<-list.files(path=path.fail.18,pattern="_only.dat")
namo<-str_replace_all(filo,pattern="_only.dat","_2018")

filu<-list.files(path=path.success.18,pattern="_only.dat")
namu<-str_replace_all(filu,pattern="_only.dat","_2018")




DATA<-NULL


for (i in 1:length(fil)){
  print(i)
  
  dat<-read.table(paste(path.fail.15,fil[i],sep=""),header=T,sep=",",dec=".")
  dat[,1]<-as.factor(rep(str_sub(fil[i], 1, 4),nrow(dat)))
  dat$Var<-str_replace_all(nam[i],pattern="fail_","")
  if(nam[i]!=nam[1]){
  dat<-subset(dat,dat$x>=0)}
  assign(nam[i],dat)
  
  daty<-read.table(paste(path.succes.15,fila[i],sep=""),header=T,sep=",",dec=".")
  daty[,1]<-as.factor(rep(str_sub(fila[i], 1, 4),nrow(daty)))
  daty$Var<-str_replace_all(nama[i],pattern="success_","")
  if(nama[i]!=nama[1]){
  daty<-subset(daty,daty$x>=0)}
  assign(nama[i],daty)
  
  DATA<-rbind.fill(DATA,dat,daty)
  
}

for (j in 1:length(filu)){
  print(j)
  dato<-read.table(paste(path.fail.18,filo[j],sep=""),header=T,sep=",",dec=".")
  dato[,1]<-as.factor(rep(str_sub(filo[j], 1, 4),nrow(dato)))
  dato$Var<-str_replace_all(namo[j],pattern="fail_","")
  if(namo[j]!=namo[1]){
  dato<-subset(dato,dato$x>=0)}
  assign(namo[j],dato)
  
  datu<-read.table(paste(path.success.18,filu[j],sep=""),header=T,sep=",",dec=".")
  datu[,1]<-as.factor(rep(str_sub(filu[j], 1, 4),nrow(datu)))
  datu$Var<-str_replace_all(namu[j],pattern="success_","")
  if(namu[j]!=namu[1]){
  datu<-subset(datu,datu$x>=0)}
  assign(namu[j],datu)
  
  DATA<-rbind.fill(DATA,dato,datu)
  
  }

library(viridis)

pal<-inferno(2,begin=0.2,end=0.8)

 tiff("Figures/Figure 4 - Response curves.tiff",
     res=500,width=4000,height=2500)
par(mfrow=c(2,4))
#SST
par(mar=c(4,3,0.5,0))
plot(fail_SST_2015$x,fail_SST_2015$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlim=c(0,25),  ylim=c(0,0.8), main="",xlab="",col=pal[2])
lines(success_SST_2015$x,success_SST_2015$y,col=pal[1])
lines(fail_SST_2018$x,fail_SST_2018$y,col=pal[2],lty=2)
lines(success_SST_2018$x,success_SST_2018$y,col=pal[1],lty=2)
axis(1,at=seq(0,25,5),pos=0)
axis(2,at=seq(0,0.8,0.2),pos=0,las=1)
mtext(side=1,"SST (°C)",line=1.75,cex=0.8)
mtext(side=3,"a)",line=2,adj=1,cex=0.9)
mtext(side=2,"Probability of suitability",line=2,adj=0.5,cex=0.9)

#3
par(mar=c(4,2,0.5,0))
plot(-fail_Bathym_2015$x,fail_Bathym_2015$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlim=c(0,6000),  ylim=c(0,0.8), main="",xlab="",col=pal[2])
lines(-success_Bathym_2015$x,success_Bathym_2015$y,col=pal[1])
lines(-fail_Bathym_2018$x,fail_Bathym_2018$y,lty=2,col=pal[2])
lines(-success_Bathym_2018$x,success_Bathym_2018$y,lty=2,col=pal[1])
axis(1,at=seq(0,6000,1500),pos=0)
axis(2,at=seq(0,0.8,0.2),pos=0,las=1,labels=F)
mtext(side=1,"Bathymetry (m)",line=1.75,cex=0.8)
mtext(side=3,"b)",line=-2,adj=0.1,cex=0.9)

#2
indic<-indic2<-paste("CHLA (g.cm\u207b\u00b3)",sep="")

par(mar=c(4,2,0.5,0))
plot(fail_CHLA_2015$x,fail_CHLA_2015$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlim=c(0,2),  ylim=c(0,0.8), main="",xlab="",col=pal[2])
lines(success_CHLA_2015$x,success_CHLA_2015$y,col=pal[1])
lines(fail_CHLA_2018$x,fail_CHLA_2018$y,col=pal[2],lty=2)
lines(success_CHLA_2018$x,success_CHLA_2018$y,col=pal[1],lty=2)
axis(1,at=seq(0,2,0.4),pos=0)
axis(2,at=seq(0,0.8,0.2),pos=0,las=1,labels=F)
mtext(side=1,indic,line=1.75,cex=0.8)
mtext(side=3,"c)",line=-2,adj=0.1,cex=0.9)



#4
par(mar=c(4,2,0.5,0))
plot(fail_gSST_2015$x,fail_gSST_2015$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlim=c(0,30),  ylim=c(0,0.8), main="",xlab="",col=pal[2])
lines(success_gSST_2015$x,success_gSST_2015$y,col=pal[1])
axis(1,at=seq(0,30,6),pos=0)
axis(2,at=seq(0,0.8,0.2),pos=0,las=1,labels=F)
mtext(side=1,"SST gradient",line=1.75,cex=0.8)
mtext(side=3,"d)",line=-2,adj=0.1,cex=0.9)


#5
par(mar=c(4,3,0.5,0))
plot(fail_gCHLA_2015$x,fail_gCHLA_2015$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlim=c(0,60),  ylim=c(0,0.8), main="",xlab="",col=pal[2])
lines(success_gCHLA_2015$x,success_gCHLA_2015$y,col=pal[1])
lines(success_gCHLA_2018$x,success_gCHLA_2018$y,lty=2,col=pal[1])
lines(fail_gCHLA_2018$x,fail_gCHLA_2018$y,col=pal[2],lty=2)
axis(1,at=seq(0,60,12),pos=0)
axis(2,at=seq(0,0.8,0.2),pos=0,las=1,labels=T)
mtext(side=1,"CHLA gradient",line=1.75,cex=0.8)
mtext(side=3,"e)",line=-2,adj=0.1,cex=0.9)
mtext(side=2,"Probability of suitability",line=2,adj=0.5,cex=0.9)


#6

indic2<-paste("EKE (m.s\u207b\u00b9)",sep="")
par(mar=c(4,2,0.5,0))
plot(fail_EKE_2015$x,fail_EKE_2015$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlim=c(0,1.5),  ylim=c(0,0.8), main="",xlab="",col=pal[2])
lines(success_EKE_2015$x,success_EKE_2015$y,col=pal[1])
lines(fail_EKE_2018$x,fail_EKE_2018$y,lty=2,col=pal[2])
lines(success_EKE_2018$x,success_EKE_2018$y,lty=2,col=pal[1])
axis(1,at=seq(0,1.5,0.25),pos=0)
axis(2,at=seq(0,0.8,0.2),pos=0,las=1,labels=F)
mtext(side=1,indic2,line=1.75,cex=0.8)
mtext(side=3,"f)",line=-2,adj=0.1,cex=0.9)


indic3<-paste("Wind speed (m.s\u207b\u00b9)",sep="")
par(mar=c(4,2,0.5,0))
plot(fail_Wind_2018$x,fail_Wind_2018$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlim=c(0,15),  ylim=c(0,0.8), main="",xlab="",col=pal[2],lty=2)
lines(success_Wind_2018$x,success_Wind_2018$y,col=pal[1],lty=2)
axis(1,at=seq(0,15,3),pos=0)
axis(2,at=seq(0,0.8,0.2),pos=0,las=1,labels=F)
mtext(side=1,indic3,line=1.75,cex=0.8)
mtext(side=3,"g)",line=-2,adj=0.1,cex=0.9)

plot(fail_Wind_2018$x,fail_Wind_2018$y,type="l",bty="n",xaxt="n",yaxt="n",ylab="",
     xlab="",col="white")
legend(x=-1,y=0.4,lty=c(1,1,2,2),col=c(pal,pal),box.lty=0,cex=1.2,
       legend=c("success 2015","fail 2015","success 2018","fail 2018"))


dev.off()
