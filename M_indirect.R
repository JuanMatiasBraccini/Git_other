#Script for calculating natural mortality based in indirect methods

#Reference: Kenchington 2013
#W.G is the weight given to methods using growth parameters
#W.noG is the weight given to methods not using growth parameters

Dry.w=0.2   #convertion wet to dry weight (Cortes 2002)

M.fun=function(A,k,Linf,Aver.T,age.mat,to,bwt,awt,W.G,W.noG)
{
  #.number of age classes
  age=0:A      
  
  #STEP 1. calculate M from different methods (see Kenchington 2013)
  #.age invariant
  #Jensen (1996)
  m.Jensen.1=1.5*k
  m.Jensen.1=rep(m.Jensen.1,length(age))
  
  m.Jensen.2=1.65/age.mat
  m.Jensen.2=rep(m.Jensen.2,length(age))
  
  #Pauly (1980)  
  m.Pauly=10^(-0.0066-0.279*log10(Linf)+0.6543*log10(k)+0.4634*log10(Aver.T))
  m.Pauly=rep(m.Pauly,length(age))
  
  #Hoenig (1983), combined teleost and cetaceans    
  m.Hoenig=exp(1.44-0.982*log(A))      
  m.Hoenig=rep(m.Hoenig,length(age))
  
  #Rikhter & Evanov (1976)
#  m.RikEf=(1.521/(age.mat^0.72))-0.155
#  m.RikEf=rep(m.RikEf,length(age))
  
  #Frisk et al (2001)
  m.Frisk=0.436*k^0.42
  m.Frisk=rep(m.Frisk,length(age))
  

  
  #.age dependent
  #Peterson and Wroblewski 1984 (dry weight in grams)
  wet.weight=1000*bwt*total.length^awt
  m.PetWro=1.92*(wet.weight*Dry.w)^-0.25
  
  #Lorenzen 1996 (weight in grams)
  m.Lorenzen=3*wet.weight^-0.288
  
  #Gislason et al (2010) (weight in grams)
  m.Gislason=1.73*(mid.FL.fem^-1.61)*(Linf^1.44)*k
  if(m.Gislason[1]>1)m.Gislason=rep(NA,length(age))
  
  #(Chen & Watanabe 1989)
#   TM=-(1/k)*log(1-exp(k*to))+to
#   TM=min(TM,age.mat)      #for duskies and sandbars TM> max.age! 
#   #(though using original TM makes not much difference)
#   ao=1-exp(-k*(TM-to))
#   a1=k*exp(-k*(TM-to))
#   a2=-0.5*(k^2)*exp(-k*(TM-to))
#   m.Chen=ifelse(age<=TM,k/(1-exp(-k*(age-to))),
#                 ifelse(age>TM,k/(ao+a1*(age-TM)+a2*(age-TM)^2),NA))
#   m.Chen=if(length(m.Chen[m.Chen<0])>0)k/(1-exp(-k*(age-to)))else{m.Chen} #fix effect of Whiskery max age>15 
  
  #STEP 2. get mean at age
  nat.mort=data.frame(m.Jensen.2,m.Pauly,m.Hoenig,m.PetWro,m.Lorenzen,m.Gislason)
  N=ncol(nat.mort)
  nn=length(age)
  
  Weights=data.frame(W.J=rep(W.noG,nn),W.P=rep(W.G,nn),W.H=rep(W.noG,nn),
                     W.P=rep(W.noG*1,nn),W.L=rep(W.noG*1,nn),W.G=rep(W.G*1,nn))
    
  nat.mort=data.frame(nat.mort,Weights)
  nat.mort=apply(nat.mort, 1, function(x) weighted.mean(x[1:N], x[(N+1):ncol(nat.mort)],na.rm = T))

  
  return(nat.mort)
}

