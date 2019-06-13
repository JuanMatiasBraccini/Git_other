library(diagram)
fn.flow.chart=function(lab,SHPE,X.coor,Y.coor,SX,ARRw,CEX,n,n1)
{
  N.shp=length(lab)
  sx=SX
  
  #define shape size
  pin   <- par ("pin")        # size of plotting region, inches 
  sy=rep(NA,N.shp)
  for(i in 1:N.shp)
  {
    if(SHPE[i]=="diamond") sx[i]=1.3*sx[i]
    if(SHPE[i]=="oval") sx[i]=1.3*sx[i]
    sy[i]=sx[i]*pin[1]/pin[2]*0.2
  }
  
  #position of shapes
  elpos=matrix(c(X.coor,Y.coor),ncol=2,byrow=F)
  Side.backpos=elpos[n:n1,]
  #create plotting area
  openplotmat(main="",cex.main=1)  
  
  #Draw arrows
  for (i in 1:(N.shp))
  {
    if(ARRw[i]=="Straight") straightarrow(elpos[i,],elpos[i+1,],arr.pos=.575)
    if(ARRw[i]=="Side.left") bentarrow(from = elpos[i,],to = elpos[i+1,],path="V",arr.pos=.4)
    if(ARRw[i]=="Side.right") bentarrow(from = elpos[i,],to = elpos[i+1,],path="H",arr.pos=.4)
    if(ARRw[i]=="Side.back") bentarrow(from = Side.backpos[1,],to = Side.backpos[2,],path="V",arr.pos=.4)
  }
  
  
  #draw shapes
  for (i in 1:N.shp)
  {
    if(SHPE[i]=="round")   textround(elpos[i,],sx[i],sy[i],lab=lab[[i]],cex=CEX)    
    if(SHPE[i]=="oval")    textellipse(elpos[i,],sx[i],sy[i],lab=lab[[i]],cex=CEX)
    if(SHPE[i]=="hexagon") texthexa(elpos[i,],sx[i],sy[i],lab=lab[[i]],cex=CEX)
    if(SHPE[i]=="square")  textrect(elpos[i,],sx[i],sy[i],lab=lab[[i]],cex=CEX)
    if(SHPE[i]=="diamond") 
    {
      textdiamond(elpos[i,],sx[i],sy[i],lab=lab[[i]],cex=CEX)
      textplain(elpos[i,],lab="No",adj=c(2,3.5))
      textplain(elpos[i,],lab="Yes",adj=c(-1,3.5))
    }
    
  }
  
} 