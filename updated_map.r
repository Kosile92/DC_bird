# import files separately
###files will now be read in one by one and clean####

###GW data###
gw <- read.csv('gw_data.csv', row.names = 1)
head(gw)
#format dates for gw data
gw$Date <- as.Date(gw$Date, format = "%d-%b-%y")

#Convert longitude and latitude decimal degrees to decimal minutes using a formula
#converted longitude and latitudes are turned into numeric data
long <- unlist(lapply(strsplit(gw$Longitude, "°"), "[[", 2))
long <- gsub("'W", "", long)
long <- as.numeric(long)
long <- long / 60
gw$long <- -77 - round(long, 5)

#Latitudes$gw
lat <- unlist(lapply(strsplit(gw$Latitude, "°"), "[[", 2))
lat <- gsub("'N", "", lat)
lat <- as.numeric(lat)
lat <- lat / 60
gw$lat <- 38 + round(lat, 5)

#bind combined data together and return it back into gw variable name
gw <- gw[, c("long", "lat", "Date", "Survey_Type")]
names(gw)[1:2] <- c("Longitude", "Latitude")


################################################################

##audubon data##
audubon <- read.csv('audubon_data.csv', row.names = 1)
head(audubon)
audubon$Date <- as.Date(audubon$Date, format = "%m/%d/%y")

#format longitudes and latitudes
audubon$Longitude <- gsub("W", "-", audubon$Longitude)
audubon$Latitude <- gsub("N", "", audubon$Latitude)

################################################################

##nat geo data##
nat_geo <- read.csv('nat_geo_data.csv', row.names = 1)
head(nat_geo)

#Date already in default format
nat_geo$Date <- as.Date(nat_geo$Date)

#Look at range of longitude values for out of range data
summary(nat_geo$Longitude)
nat_geo[which(nat_geo$Longitude >= 0), ]

#Three points appear to have the lat long numbers switched
#These can either be removed or swapped
swap <- nat_geo[which(nat_geo$Longitude >= 0), ]

nat_geo$Latitude[which(nat_geo$Longitude >= 0)] <- swap$Longitude
nat_geo$Longitude[which(nat_geo$Longitude >= 0)] <- swap$Latitude

#recheck the swap to confirm it happened
nat_geo[which(nat_geo$Longitude >= 0), ]
#######################################################################

# bind all data into data frame
data_frame <- rbind(gw, audubon, nat_geo)

# filtering for data points on or after january 2010 and just "transect" studies
my_clean_data <- subset(data_frame, Date >= "2010-01-01")
my_clean_data <- subset(my_clean_data, Survey_Type == "transect")
my_clean_data <- subset(my_clean_data, Survey_Type != "nontransect")


#write cleaned data into a new csv file
write.csv(my_clean_data, file = "my_clean_data.csv",row.names=FALSE)

#map plotting template
library(sp)
library(rgdal)

clean_data <- read.csv("my_clean_data.csv")

plotting_data <- SpatialPoints(clean_data[, c("Longitude", "Latitude")])

#Map of DC neighborhoods from maps2.dcgis.dc.gov
dc <- readOGR("Neighborhood_Clusters.shp", "Neighborhood_Clusters")

#Plot the map of DC
par(mar = c(1, 1, 1, 1))

plot(
  dc,
  col = "darkgrey",
  border = "white",
  main = "District of Columbia Bird Sightings"
)
plot(dc[46, ],
     add = TRUE,
     col = "#718BAE80",
     border = "white")


#Add your data
plot(plotting_data,
     add = TRUE,
     pch = 16,
     cex = 0.25)