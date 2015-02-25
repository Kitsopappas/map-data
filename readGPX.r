library(ggplot2)
library(plotKML)
file <- readGPX("R/maps/student3/gpx/user6ba.gpx",metadata=TRUE,bounds=TRUE,waypoint=TRUE,tracks=TRUE,routes=TRUE)
data6 <- file$tracks



# merge two data frames
#student1(big_data_ab,big_data_ba)
#student2(big_data2_ab, big_data2_ba)
#student3(big_data3_ab,big_data3_ba)


# Make lat/long data numeric
big_data3_ba$lat <- as.numeric(as.character(big_data3_ba$lat))
big_data3_ba$lon <- as.numeric(as.character(big_data3_ba$lon))

# Remove points that are not physically possible
notcomplete <- big_data3_ba[!complete.cases(big_data3_ba$lat, big_data3_ba$lon), ]
dat <- big_data3_ba[complete.cases(big_data3_ba$lat, big_data3_ba$lon), ]
notpossible <- big_data3_ba[!abs(big_data3_ba$lat) <= 90 | !abs(big_data3_ba$lon) <= 180, ]
dat <- big_data3_ba[abs(big_data3_ba$lat) <= 90, ]
dat <- big_data3_ba[abs(big_data3_ba$lon) <= 180, ]

# Remove points at lat 0 & long 0, these are very likely wrong
dat <- dat[ !dat$lat == 0 & !dat$lon == 0, ]



#create nice plot
data <- qplot(lon, lat, data=dat, shape="point",
              main="Map",
              xlab="Longitude", ylab="Latitude")

# White background and black grid lines
data + theme_bw()

# Large brown bold italics labels
# and legend placed at top of plot 
data + theme(axis.title=element_text(face="bold",
                                     size="12", color="#468499"), legend.position="top") 

#"R/data/maps/akis/gpx/user1/user1ba.gpx"