#Bubble Plot
bubble.plot=function(x,y,z,scale=1,xlab="Year",ylab="Age",...)  #This is a generic function. X, Y, and Z are the axis of the bubble plot.
  #They have no values. Values will be assigned in VPA.R code
{
  n=length(x); ny=length(y)
  xo=outer(x,rep(1,length=length(y)))
  yo=t(outer(y,rep(1,length=length(x))))
  zo=z/max(z,na.rm=T)*scale #apply(z,2,"/",rowSums(z))*length(y)*scale		#na.rm removes NA values
  
  matplot(xo,yo,type="n",xlab=xlab,ylab=ylab)
  abline(v=pretty(x),lty=3,col="black")
  for(i in 1:n){
    points(xo[i,],yo[i,],cex=zo[i,],pch=16,col="steelblue")
    points(xo[i,],yo[i,],cex=zo[i,],pch=1,col="black")
  }
}
