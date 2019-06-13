#generalised figure function for SoFaR
fun.fig.SoFar=function(DAT,DAT1,scaler,TITLE1,TITLE2,INT,INT2)
{
  MAX=max(DAT[,2],na.rm=T)/scaler
  FInYEAR=as.character(unique(DAT$finyear))
  N=length(FInYEAR)
  
  #id=match(start.yr,FInYEAR)
  id=which.min(is.na(DAT[,2]))
  FInYEAR=FInYEAR[id:length(FInYEAR)]
  NN=length(FInYEAR)
  plot(1:NN,DAT[id:N,2]/scaler,type='l',col="grey80",ylim=c(0,MAX),xaxt='n',yaxt='n',
       ylab="", xlab="",las=1,lwd=2,cex.lab=2.5)
  axis(1,at=1:NN,labels=F,tck=-0.01)
  axis(1,at=seq(1,NN,5),labels=FInYEAR[seq(1,NN,5)],tck=-0.02,cex.axis=1.35)
  
  axis(2,at=seq(0,MAX,INT),labels=F,tck=-0.01)
  axis(2,at=seq(0,MAX,INT2),labels=seq(0,MAX,INT2),tck=-0.02,cex.axis=1.35)
  mtext(TITLE1,2,las=3,cex=CeX,line=3.2)
  if(DO=="YES") mtext(TITLE2,1,cex=CeX,line=WHere)
  
  for(i in 1:(ncol(DAT1)-1))points(1:NN,DAT1[id:N,i+1]/scaler,type=TYPE[i],lty=LINE[i],col=COL,lwd=1.5,pch=PCH,bg=BG[i])
}