WAcoast<-read.table("C:/Matias/Data/Mapping/WAcoastPointsNew.txt", header=T)
WAislands<-read.table("C:/Matias/Data/Mapping/WAislandsPointsNew.txt", header=T)
Shelf<-read.table("C:/Matias/Data/Mapping/shelf.txt", header=T)

plotmap <- function(long, lat, PLATE,COLORE,XLIM,YLIM)
  {
    plot(long, lat, type='n', main="", axes=F, xlab=" ", ylab="",plt = PLATE,xlim=XLIM,ylim=YLIM,
         xaxs="i",yaxs="i") 
    polygon(WAcoast$Longitude,WAcoast$Latitude, col=COLORE)
    for (j in 1: length(unique(WAislands$ID))){
      polygon(WAislands$Longitude[WAislands$ID==unique(WAislands$ID)[j]],
      WAislands$Latitude[WAislands$ID==unique(WAislands$ID)[j]], col=COLORE)}
    par(new=T) 
    plot(long, lat, type='n', main="", axes=F, xlab=" ", ylab="",plt = PLATE,xlim=XLIM,ylim=YLIM,
         xaxs="i",yaxs="i") 
    box()
  
}