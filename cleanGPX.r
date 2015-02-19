library(ggplot2)
library(plotKML)
library(pracma)
file <- readGPX("R/maps/student3/gpx/user6ba.gpx",metadata=TRUE,bounds=TRUE,waypoint=TRUE,tracks=TRUE,routes=TRUE)
data <- file$tracks
data <- data[[1]]$`Kostas-BA-user6`
len <- length(data$lon)

#############
#Haversine_formula
getDistance <- function(lat1,lon1,lat2,lon2) {
  R <- 6.3710e+6; #Radius of the earth in m
  dLat <- deg2rad(lat2-lat1) #deg2rad below
  dLon <- deg2rad(lon2-lon1)
  a <- sin(dLat/2) * sin(dLat/2) +
  cos(deg2rad(lat1)) * cos(deg2rad(lat2)) * 
  sin(dLon/2) * sin(dLon/2)
  
  c <- 2 * atan2(sqrt(a), sqrt(1-a))
  d <- R * c #Distance in m
  return(d)
}

#############
#Time difference of %d secs
findTimeDif <- function(start_time,end_time){
  dif <- strptime(end_time, "%Y-%m-%dT%H:%M:%OSZ") - strptime(start_time, "%Y-%m-%dT%H:%M:%OSZ")
  dif <- as.numeric(dif) 
  return(dif)
}

#############
#for loop data cleanning
  
distance <- getDistance(data$lat[seq(1, length(data$lon), 2)],data$lon[seq(1, length(data$lon), 2)],data$lat[seq(2, length(data$lon), 2)],data$lon[seq(2, length(data$lon), 2)])
timeDiff <- findTimeDif(data$time[seq(1, length(data$lon), 2)],data$time[seq(2, length(data$lon), 2)])
div <- distance/timeDiff
div
x <- 1
y <- 0
while(div[seq(1,length(div))]>1.3){
  x
}

  


#"R/data/maps/akis/gpx/user1/user1ba.gpx"
