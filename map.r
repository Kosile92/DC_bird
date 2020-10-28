# import files 
firstfile <- read.csv(file.choose())
secondfile <- read.csv(file.choose())
thirdfile <- read.csv(file.choose())

#install measurements package
library(measurements)

#firstfile cleaning (audubon data)
firstfile <- na.omit(firstfile)
firstfile$Longitude = gsub('W', '', firstfile$Longitude)
firstfile$Longitude = measurements::conv_unit((firstfile$Longitude), from = 'deg_dec_min', to = 'dec_deg')
firstfile$Latitude = gsub('N', '', firstfile$Latitude)
firstfile$Latitude = measurements::conv_unit((firstfile$Latitude), from = 'deg_dec_min', to = 'dec_deg')
firstfile$Date <- as.Date(firstfile$Date)
firstfile$Date <- as.Date(firstfile$Date, format="%d-%b-%y")
firstfile$Date <- format(firstfile$Date , format = "%m/%d/%y")
firstfile$Longitude <- as.numeric(firstfile$Longitude)
firstfile$Latitude <- as.numeric(firstfile$Latitude)

#second file cleaning (gw data)
secondfile <- na.omit(secondfile)
secondfile$Longitude = gsub('°', ' ', secondfile$Longitude)
secondfile$Longitude = gsub("'W", '', secondfile$Longitude)
secondfile$Longitude = measurements::conv_unit((secondfile$Longitude), from = 'deg_dec_min', to = 'dec_deg')
secondfile$Latitude = gsub('°', ' ', secondfile$Latitude)
secondfile$Latitude = gsub("'N", '', secondfile$Latitude)
secondfile$Latitude = measurements::conv_unit((secondfile$Latitude), from = 'deg_dec_min', to = 'dec_deg')
secondfile$Date <- as.Date(secondfile$Date)
secondfile$Date <- as.Date(secondfile$Date, format="%d-%b-%y")
secondfile$Date <- format(secondfile$Date, format = "%m/%d/%y")
secondfile$Longitude <- as.numeric(secondfile$Longitude)
secondfile$Latitude <- as.numeric(secondfile$Latitude)

#thirdfile cleaning (nat geo data)
thirdfile <- na.omit(thirdfile)
thirdfile$Longitude = measurements::conv_unit((thirdfile$Longitude), from = 'deg_dec_min', to = 'dec_deg')
thirdfile$Latitude = measurements::conv_unit((thirdfile$Latitude), from = 'deg_dec_min', to = 'dec_deg')
thirdfile$Date <- as.Date(thirdfile$Date)
thirdfile$Date <- format(thirdfile$Date, format = "%m/%d/%y")
thirdfile$Longitude <- as.numeric(thirdfile$Longitude)
thirdfile$Latitude <- as.numeric(thirdfile$Latitude)

# bind all data into data frame
data_frame <- rbind(firstfile, secondfile, thirdfile)
data_frame$Date <- format(data_frame$Date, format="%d-%b-%y")
data_frame$Date <- as.Date(data_frame$Date)
data_frame$Date <- as.Date(data_frame$Date, format="%m/%d/%y")
data_frame$Date <- format(data_frame$Date, format = "%m/%d/%y")

# filtering for data points on or after january 2010 and just "transect" studies
my_clean_data1 <- subset(data_frame, Date >= "2010-01-01")
my_clean_data <- subset(my_clean_data1, Survey_Type == "transect")

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