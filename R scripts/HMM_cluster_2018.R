load("Data/NewlyCreatedData/clean_interp_loc_2018.RData")


essai.2018<-loc.interpolated.2018
#get a travelID by day
essai.2018$ID<-essai.2018$TravelID

proc.essai<-prepData(essai.2018,type="LL",
                     coordNames=c("Longitude","Latitude")) #step in km if LL, m if UTMe")) #step in km if LL, m if UTM

par(mfrow=c(1,2))
hist(proc.essai$step)
hist(proc.essai$angle)

 #test for von mise distribution for angle concentrationsons
for (foo in 1:nruns){
 print(foo)
  mu0<-sort(c(runif(1,0.3,7),runif(1,1,15),runif(1,5,25)) ) # means for step length
  sigma0<-c(runif(1,0.3,10),runif(1,0.3,25),runif(1,0.3,10))  #sd for step length
  stepPar0<-c(mu0,sigma0)  #joint object for step length
  angleMean0<-sample(c(0,pi),size=3,replace=T)   #mean for turning angle
  kappa0<-runif(3,0,5)  # angle concentration
  anglePar0<-c(angleMean0,kappa0)

  print(c(stepPar0,anglePar0))

  m0<-fitHMM(data=proc.essai,nbStates=3,stepPar0=stepPar0,anglePar0=anglePar0,
             formula=~1,angleDist="vm")
  mles[foo]<--m0$mod$minimum

  print( mles[foo])
}

table(round(mles))


 #test for wrapped cauchy for angle concentrations
  set.seed(12345)
for (foo in 1:nruns){
  print(foo)
  mu0<-sort(c(runif(1,0.3,7),runif(1,1,15),runif(1,5,25)) ) # means for step length5,30)) ) # means for step length
  sigma0<-c(runif(1,0.3,10),runif(1,0.3,25),runif(1,0.3,10))  #sd for step length0.3,10))  #sd for step length
  zeromass0<-runif(3,0,0.01)
  stepPar0<-c(mu0,sigma0,zeromass0)  #joint object for step lengthor step length
  angleMean0<-sample(c(0,pi),size=3,replace=T)   #mean for turning anglean for turning angle
  kappa0<-runif(3,0,5)  # angle concentration
  anglePar0<-c(angleMean0,kappa0)

  print(c(stepPar0,anglePar0))

  m0<-fitHMM(data=proc.essai,nbStates=3,stepPar0=stepPar0,anglePar0=anglePar0,pPar0,anglePar0=anglePar0,
             formula=~1,angleDist="wrpcauchy")
  mles[foo]<--m0$mod$minimum

  print( mles[foo])
}

table(round(mles))



 
 
 
 