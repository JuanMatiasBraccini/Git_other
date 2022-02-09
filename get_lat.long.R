#script for extraction latitude and longitude for a list of locations
#note: input dataframe must have Cityname and CountryCode

library(RJSONIO)
get_lat.long=function(test)
{
  nrow <- nrow(test)
  counter <- 1
  test$lon[counter] <- 0
  test$lat[counter] <- 0
  while (counter <= nrow){
    CityName <- gsub(' ','%20',test$Cityname[counter]) #remove space for URLs
    CountryCode <- test$CountryCode[counter]
    url <- paste(
      "http://nominatim.openstreetmap.org/search?city="
      , CityName
      , "&countrycodes="
      , CountryCode
      , "&limit=9&format=json"
      , sep="")
    x <- fromJSON(url)
    if(is.vector(x)){
      test$lon[counter] <- x[[1]]$lon
      test$lat[counter] <- x[[1]]$lat    
    }
    counter <- counter + 1
  }
  return(test)
}