
#Script for sourcing temperature data from NOAA
library(RNetCDF)
library(reshape2)

if(!exists('handl_OneDrive')) source('C:/Users/myb/OneDrive - Department of Primary Industries and Regional Development/Matias/Analyses/SOURCE_SCRIPTS/Git_other/handl_OneDrive.R')
setwd(handl_OneDrive("Data/Reynolds SST"))
url <- 'ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc'
destfile <- 'sst.mnmean.nc'
download.file(url, destfile, mode = "wb")

nc1 <- open.nc('sst.mnmean.nc') 
print.nc(nc1)
tim <- var.get.nc(nc1, 'time')   #"days since 1800-1-1 00:00:00"
dates <-  as.Date(tim, origin="1800-1-1 00:00:00")
z <- strptime(dates, "%Y-%m-%d")
month <- z$mon+1
year <- z$year+1900
lat <- var.get.nc(nc1, 'lat')
lats <-which(lat<=-21.5 & lat>=-35)
lats1 <- lat[lats]
lon <- var.get.nc(nc1, 'lon')
lons <- which(lon>=112.5 & lon<=116.5)
lons1 <- lon[lons]
sst <- var.get.nc(nc1, 'sst') * 0.01
sst1 <- sst[lons,lats,]
(sst1[,,2])
dimnames(sst1) <- list(lon=lons1, lat=lats1, date=dates)
sst2 <- melt(sst1)
sst2$date <- dates
z <- strptime(sst2$date, "%Y-%m-%d")
sst2$month <- z$mon+1
sst2$year <- z$year+1900
tail(sst2)

write.table(sst2, 'Coast_temperatures.csv', row.names=F, col.names=T, sep=',')
