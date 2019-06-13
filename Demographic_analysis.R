#Demographic analysis script

#note: simple script for calculating demographic parameters using leslie matrices
#      monte carlo simulations included (as per Reference Points)

library(triangle)
library(TeachingDemos)      #for grey scale
library(MASS)      #for multivariate normal pdf
library(popbio)     #for solving matrices

#source sigmas for var-covar matrix
source("C:/Matias/Analyses/Reference Points/5.Derive var_covar Matrix.r")

#source indirect estimation of M
source("C:/Matias/Analyses/SOURCE_SCRIPTS/M_indirect.R")



#PARAMETERS

A.min=0

#Life history par vectors (Whiskery,Gummy) Gummy from Walker 2010
#Min values
Min.max.age=c(15,15)
Min.breed.freq=c(1,1)
Min.age.mat=c(6,5)
Min.fec=c(4,1)       #Mean fec Whiskery set to satisfy the mean of 0.3417*(100:135)-17.8 (Simpfendorfer 1998)
                      #gummy actually has a strong power relation(1.12*exp(-3.033+(0.00396*(1000:1800)))   (Walker 2010)
#Max values
Max.max.age=c(20,20) #put +1 because using floor in random sample
Max.breed.freq=c(0.5,1)
Max.age.mat=c(8,9) #put +1 because using floor in random sample
Max.fec=c(28,57)

species.names=c("whiskery","gummy")
id=match(species.names,names(SIGMA))
SIGMA=SIGMA[id]                  

species.list <-vector("list", length(species.names))
names(species.list) <- species.names
N.sp=length(species.list)

pars.names <- c("max.age", "M","fec","breed.freq","age.mat")
pars.list <- vector("list", length(pars.names))
names(pars.list) <- pars.names

#Fill in species list of pars
for (i in 1:N.sp)
{
  species.list[[i]]=list(max.age=c(Min.max.age[i],Max.max.age[i]),fec=c(Min.fec[i],Max.fec[i]),
                         breed.freq=c(Min.breed.freq[i],Max.breed.freq[i]),
                         age.mat=c(Min.age.mat[i],Max.age.mat[i]))
}


#Average water temperature
Temperature=c(18,18)

#Growth pars (female)
#Linf.w=120.7 #Simfendorfer et al 200, age and growth
Linf.w=130
Linf.g=202

#K.w=0.369
K.w=0.1
K.g=0.086

to.w=-0.6
to.g=-3


#Size at birth
Size.birth=c(25,25)

#FL to TL pars             
b.w=8.891
b.g=8.891

a.w=1.046
a.g=1.046



#TL to TW pars 
bwt.w=0.0000163
bwt.g=0.0000163

awt.w=2.733
awt.g=3


#Put pars in data frame for loop
Growth.pars=data.frame(Linf=c(Linf.w,Linf.g),K=c(K.w,K.g),to=c(to.w,to.g))
Length.conv=data.frame(b=c(b.w,b.g),a=c(a.w,a.g))
Weight.conv=data.frame(bwt=c(bwt.w,bwt.g),awt=c(awt.w,awt.g))




#PROCEDURE
#1. Priors
#1.1. Natural mortality
#source external function 
Wght.G=c(1,1)    # weight given to methods using growth parameters (high uncertainty for whiskery so give lower weight)
Wght.noG=c(10,1)  # weight given to methods not using growth parameters
#Wght.noG=c(10,1)

#1.2 Max age
A.max=function(MIN,MAX,MODE) floor(rtriangle(1, a=MIN, b=MAX, c=MODE))

#1.3 Fecundity
FEC=function(MIN,MAX,MODE) round(rtriangle(1, a=MIN, b=MAX, c=MODE))


#1.4 Reproductive periodicity
REP.PER=function(MIN,MAX)runif(1,MIN,MAX)

#1.5 age at maturity
AGE.MAT=function(MIN,MAX)floor(runif(1,MIN,MAX))  

#1.6 growth pars (multivariate normal distribution)
GROWTH=function(Linf.mean,k.mean,sigma)
{
  growth.pars.sim=mvrnorm(1,mu=c(Linf.mean,k.mean),Sigma=sigma,tol=1e-10)
  if(growth.pars.sim[2]<=0.001)    #repeat until sensible pars obtained
  { repeat 
  {
    growth.pars.sim=mvrnorm(1,mu=c(Linf.mean,k.mean),Sigma=sigma,tol=1e-10)
    if(growth.pars.sim[2]>0.001)break
  }
  }
  return(growth.pars.sim)
}


Leslie=function(max.age,M,fec,breed.freq,sex.ratio,age.mat)
{

  #survivorship
  surv=exp(-M)
  
  #proportion surviving
   lx=rep(NA,length(age))
   lx[1]=1.0
   for (i in 2:(max.age))lx[i]=lx[i-1]*exp(-M[i] )
   lx[length(age)]=0
   
  
  
  #fecundity  
  fecundity=rep(fec*breed.freq*sex.ratio,length(age))
  
  #maturity
  maturity=plogis(age,age.mat,1)
  
  mx =fecundity*maturity 
  
  #probability of surviving (birth-pulse, post-breeding census)
  px=vector(length=length(lx))
  for(i in 2:length(lx)) px[i-1]=(lx[i])/(lx[i-1])
  #fertility  (birth-pulse, post-breeding census)
  bx=mx*px
  
  #projection matrix
  PX=px
  PX=PX[-length(PX)]
  BX=bx
  n=length(BX)
  Data=matrix(0,nrow=n,ncol=n)
  diag(Data)[-nrow(Data)]=PX
  Data=rbind(matrix(0,nrow=1,ncol=n),Data)
  Data=Data[-(n+1),]
  Data[1,]=BX
  rownames(Data)=colnames(Data)=(0+1):n
  return(Matrix=Data)
}



#MAIN

#---MAIN SECTION----
N.sim=1000


#Demopraphic analyses
LAMBDA.out=LAMBDA.reject.out=r.out=t2.out=v.out=Elast.out=vector('list',length=N.sp)
names(LAMBDA.out)=names(LAMBDA.reject.out)=names(r.out)=names(t2.out)=names(v.out)=
  names(Elast.out)=species.names

for (a in 1:N.sp)
{
  #1. Select biological parameters
  WT=Weight.conv[a,]  
  GR=Growth.pars[a,]
  TO=Growth.pars[a,3]
  SIG=SIGMA[[a]]
  Lo=Size.birth[a]
  AA=species.list[[a]]$max.age
  FF=species.list[[a]]$fec
  BF=species.list[[a]]$breed.freq
  b.fem=Length.conv[a,1]
  a.fem=Length.conv[a,2]
  sex.ratio=0.5
  AMat=species.list[[a]]$age.mat
  Temper=Temperature[a]
  w.g=Wght.G[a]
  w.ng=Wght.noG[a] 
  

  Trigged=rep(NA,N.sim)
  Proyec.matrix=vector('list',length=N.sim)
  
  # 2. Monte carlo procedure
  for (j in 1:N.sim)
  {
    #1. draw random samples of input parameters
    A.MAX=A.max(AA[1],AA[2],AA[1])
    age=A.min:A.MAX
    GR=GROWTH(GR[[1]],GR[[2]],SIG)     
    if(!a==1)Fec=FEC(FF[1],FF[2],round(mean(c(FF[1],FF[2]))))
    if(a==1)Fec=FEC(FF[1],FF[2],22)#use higher mode for whiskery
    
    Rep=REP.PER(BF[2],BF[1])
    A.MAT=AGE.MAT(AMat[1],AMat[2])    
    mid.FL.fem=Lo+(GR[1]-Lo)*(1-exp(-GR[2]*age))  #modified version
    total.length=b.fem+a.fem*mid.FL.fem
    mm=M.fun(A.MAX,GR[2],GR[1],Temper,A.MAT,TO,WT[1,1],WT[1,2],w.g,w.ng)
    
    
    #2. calculate demographic quantities 
    Proyec.matrix[[j]]= Leslie(A.MAX,mm,Fec,Rep,sex.ratio,A.MAT)
    

    #3. Repeat if nonsense outputs (e.g. r<0), i.e. obtain sensible joint priors #MISSING!!!!
    Trigger=0

    
    
 }
  
  #3. Solve projection matrix
  LAMBDA=sapply(Proyec.matrix,lambda)
  LAMBDA.reject=LAMBDA[LAMBDA<1]  #store negative growth separetely
  LAMBDA=LAMBDA[LAMBDA>=1]        #remove param combos that yield negative growth
  r.sim=log(LAMBDA)         
  t2.sim=log(2)/r.sim           #pop doubling time
  v=sapply(Proyec.matrix,reproductive.value)    #reproductive value
  Elasticities=sapply(Proyec.matrix,elasticity) #elasticities
  
  #4. Store quantities
  LAMBDA.out[[a]]=LAMBDA
  LAMBDA.reject.out[[a]]=LAMBDA.reject
  r.out[[a]]=r.sim
  t2.out[[a]]=t2.sim
  v.out[[a]]=v
  Elast.out[[a]]=Elasticities
  
}


for (a in 1:N.sp)
{
  hist(r.out[[a]],main=species.names[a],xlab='r')
  legend('topleft',c(paste("median=",round(median(r.out[[a]]),2),sep=""),
                     paste("CI=",round(quantile(r.out[[a]],probs=c(0.025,0.975)),2),sep="")),bty='n')
}
