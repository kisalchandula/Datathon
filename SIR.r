#########################################
#Suspectable,Infected and recoverd Model
##sri lanka covid 19 
########################################
S0<- 21803000 #sri lanka population
I0<- 1        #Initial infected
R0<- 0
E0<- 10 #they have virus but they don't know about that
#parameters
alpha1<-1/100000000
alpha2<-1/100000000
beta1<-1/14
gamma1<-1/7
gamma2<-1/21
out1<-matrix(0,ncol = 4,nrow = 1000)
for(i in 1:1000){
  E0n<-E0
  S0n<- S0
  I0n<- I0
  R0n<- R0
  S0<-max(0,S0n-alpha1*E0n*S0n-alpha2*S0n*I0n)
  E0<-max(0,E0n+alpha1*E0n*S0n+alpha2*S0n*I0n-gamma1*E0n-gamma2*E0n)
  I0<-max(0,I0n+gamma1*E0n-beta1*I0n)
  R0<-max(0,R0n+beta1*I0n+gamma2*E0n)
  out1[i,1] <- S0
  out1[i,2] <- E0
  out1[i,3] <- I0
  out1[i,4] <- R0
  
}
plot(1:1000,out1[,1],type="l",ylim=c(0,21803000))
lines(1:1000,out1[,2],col="orange")
lines(1:1000,out1[,3],col="red")
lines(1:1000,out1[,4],col="seagreen")