load("clean_interp_loc_2015.RData")

library(moveHMM)

essai.2015<-loc.interpolated.2015
#get a travelID by day
essai.2015<-loc.interpolated.2015
#get a travelID by day
essai.2015$ID<-essai.2015$TravelID

proc.essai<-prepData(essai.2015,type="LL",
                     coordNames=c("Longitude","Latitude")) #step in km if LL, m if UTM

#fiding the right set of parameters for step length and turning angles  for 3 states
 set.seed(12345)
 nruns<-25
 mles<-vector("numeric",length=nruns)

 #test for von mise distribution for angle concentrations
for (foo in 1:nruns){
  print(foo)
  mu0<-sort(c(runif(1,0.3,10),runif(1,1,20),runif(1,5,30)) ) # means for step length
  sigma0<-c(runif(1,0.3,10),runif(1,0.3,30),runif(1,0.3,10))  #sd for step length
  zeromass0<-runif(3,0,0.01)
  stepPar0<-c(mu0,sigma0,zeromass0)  #joint object for step length
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

#lowest likelihood: -9345 with von mise
#parameters:  mu0<-
  #sigma0<-c()  #sd for step length
  #zeromass0<-c()
 # angleMean0<-c()   #mean for turning angle
 # kappa0<-c()

 #test for wrapped cauchy for angle concentrations
for (foo in 1:nruns){
  print(foo)
  mu0<-sort(c(runif(1,0.3,10),runif(1,1,20),runif(1,5,30)) ) # means for step length
  sigma0<-c(runif(1,0.3,10),runif(1,0.3,30),runif(1,0.3,10))  #sd for step length
  zeromass0<-runif(3,0,0.01)
  stepPar0<-c(mu0,sigma0,zeromass0)  #joint object for step length
  angleMean0<-sample(c(0,pi),size=3,replace=T)   #mean for turning angle
  kappa0<-runif(3,0,5)  # angle concentration
  anglePar0<-c(angleMean0,kappa0)

  print(c(stepPar0,anglePar0))

  m0<-fitHMM(data=proc.essai,nbStates=3,stepPar0=stepPar0,anglePar0=anglePar0,
             formula=~1,angleDist="wrpcauchy")
  mles[foo]<--m0$mod$minimum

  print( mles[foo])
}

table(round(mles))