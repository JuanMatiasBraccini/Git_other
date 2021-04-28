#Script for plotting TDGDLF map and receivers
if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
fn.map.zones.receivers=function(a,PLATE,OZ.lat,OZ.long,South.WA.lat,South.WA.long)
{
  library(PBSmapping) 
  b=seq(-37,South.WA.lat[2],length.out=length(a))
  LATT=South.WA.lat[2]:South.WA.lat[1]
  LONGG=South.WA.long[1]:South.WA.long[2]
  S.WA.long=c(South.WA.long[2], South.WA.long[2], South.WA.long[1], South.WA.long[1])
  S.WA.lat=c(South.WA.lat[2], South.WA.lat[1], South.WA.lat[1], South.WA.lat[2])
  Perth=c(115.866,-31.95)
  Rotnest=c(115.50,-32.02)
  
    #bathymetry
  if(!exists("reshaped"))
  {
    Bathymetry_120=read.table(handl_OneDrive("Data/Mapping/get_data112_120.cgi"))
    Bathymetry_138=read.table(handl_OneDrive("Data/Mapping/get_data120.05_138.cgi"))
    Bathymetry=rbind(Bathymetry_120,Bathymetry_138)
    Bathymetry=Bathymetry[order(Bathymetry$V1,Bathymetry$V2),]
    xbat=sort(unique(Bathymetry$V1))
    ybat=sort(unique(Bathymetry$V2))  
    reshaped=as.matrix(reshape(Bathymetry,idvar="V1",timevar="V2",v.names="V3", direction="wide"))
  }
    
    #receivers
  AATAMS.all=read.csv(handl_OneDrive("Data/Tagging/Acoustic_tagging/Acoustic_tagging_data/AATAMS.all.csv"))
  SMN.all=read.csv(handl_OneDrive("Data/Tagging/Acoustic_tagging/Acoustic_tagging_data/SMN.all.csv"))
  Receivers=rbind(SMN.all,AATAMS.all)
  Receivers$Station=paste(Receivers$latitude,Receivers$longitude) 
  Receivers=Receivers[order(Receivers$Station),]
  STATIONS=Receivers[!duplicated(Receivers$Station),]
  STATIONS=subset(STATIONS,longitude<=150)
  
  source(handl_OneDrive("Analyses/SOURCE_SCRIPTS/Plot.Map.R"))  
  data(worldLLhigh)
  
  #Plot base map with zones
  par(mar=c(2,2,2,2),oma=c(1,1,1,1))
  plotmap(a,b,PLATE,"dark grey",South.WA.long,c(-37,-25))
  text(113.5,-29.75,("West"),col="black", cex=1.75)
  text(113.5,-30.25,("coast"),col="black", cex=1.75)
  polygon(x=c(116.5,116.5,112,112),y=c(-26.5,-33,-33,-26.5),lwd=1.5,col=rgb(.1,.1,.1,alpha=.2))
  text(114,-35.25,("Zone 1"),col="black", cex=1.75)
  polygon(x=c(116.5,116.5,112,112),y=c(-33,-37,-37,-33),lwd=1.5,col=rgb(.3,.3,.3,alpha=.5))
  text(122,-35.25,("Zone 2"),col="black", cex=1.75)
  polygon(x=c(129,129,116.5,116.5),y=c(-30,-37,-37,-30),lwd=1.5,col=rgb(.7,.7,.7,alpha=.2))
  axis(side = 1, at =LONGG, labels = F, tcl = 34,lty=2)
  axis(side = 4, at = LATT, labels = F,tcl =34,lty=2)
  contour(xbat, ybat, reshaped[,2:ncol(reshaped)],ylim=plotlat[[i]],xlim=plotlong[[i]], zlim=c(-1,-300),
        nlevels = 3,labcex=1.25,lty = c(1,2,3),col=c("gray20","gray20","gray20","transparent"),add=T)

  par(new=T,mar=c(2,2,2,2),oma=c(1,1,1,1))
  plotmap(a,b,PLATE,"dark grey",South.WA.long,c(-37,-25))
  axis(side = 1, at =seq(112,129,2), labels = seq(112,129,2), tcl = .35,las=1,cex.axis=1,padj=-1.25)
  axis(side = 2, at = seq(-36,-25,2), labels = -seq(-36,-25,2),tcl = .35,las=2,cex.axis=1,hadj=.3)
  text(116.73,Perth[2],("Perth"),col="black", cex=1.1)
  points(115.86,-31.95,pch=19)
  text(116.73,-33.55,("Bunbury"),col="black", cex=1.1)
  points(115.6,-33.55,pch=19)
  text(117.7,-34.8,("Albany"),col="black", cex=1.1)
  points(117.8,-35,pch=19)
  text(122,-33.66,("Esperance"),col="black", cex=1.1)
  points(121.9,-33.86,pch=19)
  mtext("Latitude (?S)",side=2,line=1.75,las=3,cex=1.75)
  mtext("Longitude (?E)",side=1,line=1.75,cex=1.75)
  
  #Add receivers
  points(STATIONS$longitude,STATIONS$latitude,col='firebrick3',pch=19,cex=.5)  #receiver location
  
  
  #Add Oz
  par(fig=c(.5,.92,.5,.92), new = T,mgp=c(.1,.4,0))
  plotMap(worldLLhigh, xlim=OZ.long,ylim=OZ.lat,plt = c(.1, 1, 0.075, 1),
          col="dark grey",tck = 0.025, tckMinor = 0.0125, xlab="",ylab="",axes=F)
  box()
  polygon(x=S.WA.long,y=S.WA.lat,lwd=1.5,col=rgb(.1,.1,.1,alpha=.2))
  text(134,-23.5,("Australia"),col="black", cex=2)
}