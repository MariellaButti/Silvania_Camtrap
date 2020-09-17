## Camera trap Analytics Course - ICCB, Cartagena, July 22, 2017
## Jorge Ahumada @ Conservation International

## ----Load libraries------------------------
library(TeachingDemos)
library(lubridate)
library(unmarked)
library(ggplot2)
library(dplyr)
library(chron)
library(vegan)
library(activity)
library(ggmap)
library(here)


## ----Source this file--------
source(here("silvania-temporario", "camera trap analysis code-WILDID-09-20-17.R"))

## ----Load data from Floresta Nacional de Silvania -------
silvania <- f.readin.fix.data(here("silvania-temporario", "Wild_ID_silvania.csv"))

# How many rows and columns?
dim(silvania)

#Look at the first 6 rows of data
head(silvania)
#View(silvania)

## ----Subset the data-----------------
# Which photo types do we have?
table(silvania$Class)

# Only work with the animal photos
#silvania <- filter(silvania, Photo.Type == "Animal") # not working because Photo.Type is null

## ----Number of records per camera and sampling regime--------------------
# Number of images
dim(silvania)

#Number of images per camera trap
imgsPerCT <- silvania %>% group_by(Camera.Trap.Name, Latitude, Longitude) %>% summarize(n = n()) %>% arrange(desc(n))
imgsPerCT

## ----make some plots-----------
#plot
p <- ggplot(imgsPerCT, aes(x = reorder(Camera.Trap.Name, -n), y = n)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Camera Trap Name") + ylab("Number of images")
p

## ----plot deployment in time---------------------------------------------
#Look at how camera traps were deployed
# First create a table
tabCTN_PD <- with(silvania, table(Camera.Trap.Name, Photo.Date))
head(tabCTN_PD)
#Get it ready for ggplot
tabCTN_PD <- melt(tabCTN_PD)

#Plot it
p <- ggplot(tabCTN_PD, aes(y = Camera.Trap.Name, x = Photo.Date)) + geom_raster(aes(fill=value), alpha = 0.8) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
p


# Separate data into events for different types of analysis ---------------

# Extract the data from species Sus scrofa
Sus <- filter(silvania, bin == "Sus scrofa")

# Group by events that are 30 minutes apart
Sus <- f.separate.events(Sus, 30)

# Order the data sequentially
Sus <- f.order.data(Sus)

# Calculate for each event the time
# First get the median time for each event
acTimeSus <- Sus %>% group_by(grp) %>% summarize(median = median(td.photo))

# Then extract just the hh:mm:ss information
acTimeSus <- f.extracthms(acTimeSus$median)

# Extract just the hour information - we will use later
acTimeSus_hour <- data.frame(seq = 1: length(acTimeSus), hour = hour(acTimeSus))

# Convert to radian time
acTimeSus <- f.timeformatfunc(acTimeSus)

actMod_Sus <- fitact(acTimeSus, sample = "model", reps=100)


# Plot the result ---------------------------------------------------------

plot(actMod_Sus, main = "Sus scrofa")
# Do a circular plot of activity data

# Plot results in a circular plot
ggplot(acTimeSus_hour, aes(x = hour)) + geom_histogram(breaks = seq(0, 24), colour = "grey") + coord_polar(start = 0) + theme_minimal() + ggtitle("Sus scrofa") + scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))

#---------------------------------------------------------
# Write a couple of functions to do this
calcActivity <- function(dataset, speciesName, threshold){
  # Extract the data from species
  spdata <- filter(dataset, bin == speciesName)
  
  # Group by events that are two minutes apart
  spdata <- f.separate.events(spdata, threshold)
  
  # Order the data sequentially
  spdata <- f.order.data(spdata)
  
  # Calculate for each event the time
  # First get the median time for each event
  acTime <- spdata %>% group_by(grp) %>% summarize(median = median(td.photo))
  # Then extract just the hh:mm:ss information
  acTime <- f.extracthms(acTime$median)
  # Extract the hour information for the circular plot
  acTime_hour <- data.frame(seq = 1:length(acTime), hour = hour(acTime))
  # Convert to radian time
  acTime <- f.timeformatfunc(acTime)
  # fit an activity kernel
  actModel <- fitact(acTime, sample = "model", reps=100)
  # Plot it in a couple ways
  
  plot(actModel, main = speciesName)
  
  list(actModel, acTime_hour)
}

plot.circular.activity <- function(actdata, speciesName) {
  ggplot(actdata, aes(x = hour)) + geom_histogram(breaks = seq(0, 24), colour = "grey") + coord_polar(start = 0)  + theme_minimal() + ggtitle(speciesName) + scale_x_continuous("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0, 24))
}

# Run for Sus (again, this time using the function)
actMod_Sus <- calcActivity(silvania, "Sus scrofa", 30)

# Run it for other species
actMod_Pecari <- calcActivity(silvania, "Pecari tajacu", 30)

# Do a ciruclar plot of activity
plot.circular.activity(actMod_Pecari[[2]], "Pecari tajacu")


# Compare the activity of two species
compareAct(list(actMod_Sus[[1]],actMod_Pecari[[1]]))


# Generate spatial distributions ----------------------------------

# Start with provide the lon/lat range of the data
lon <- range(silvania$Longitude)
lat <- range(silvania$Latitude)

# Extract the unique lat/lons and put them on a data frame
locationsFNS <- unique(cbind(as.character(silvania$Camera.Trap.Name), silvania$Latitude,silvania$Longitude))

locationsFNS <- data.frame(Camera.Trap.Name = locationsFNS[,1], Latitude = as.numeric(locationsFNS[,2]), Longitude = as.numeric(locationsFNS[,3]))

locationsFNS <- dplyr::arrange(locationsFNS, Camera.Trap.Name)

# If you have internet: Download the map from google
map <- get_map(location = c(c(lon[1],lat[1]),c(lon[2],lat[2])), zoom = 10, source = "google", maptype = "terrain")

# Plot the locations of the camera traps
ggmap(map, extent = "normal", maprange = T) + geom_point(data=locationsFNS, aes(x = Longitude, y = Latitude), colour="red", size = 0.1)

# Plot the number of images per camera trap point
ggmap(map, extent = "normal", maprange = T) + geom_point(data = imgsPerCT, aes(x = Longitude, y = Latitude, color = n), size = 0.5)

# Plot as a surface
ggmap(map, extent = "device", legend = "topleft")  + stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level..), data = silvania, geom = "polygon", size = 2, bins = 10, alpha = 0.5)

# Do it for Sus
# First extract the number of photographic events by camera trap
sevPerCT_Sus <- Sus %>% group_by(Camera.Trap.Name, Latitude, Longitude) %>% summarize(n = length(unique(grp)))

# Then put them in a map
ggmap(map) + geom_point(data = sevPerCT_Sus, aes(x = Longitude, y = Latitude, color = n), size = 0.5) + ggtitle("Sus scrofa")

# Can also be expressed as a relative abundance index - but CAREFUL - uncorrected for detection
ggmap(map) + geom_point(data = sevPerCT_Sus, aes(x = Longitude, y = Latitude, color = n/sum(n)), size = 0.5) + ggtitle("Sus scrofa")




##---- Species richness analysis---------------------------
unique(silvania$bin)

#Remove the blank species
# Let's do this in a copy of silvania
silvania.copy <- silvania

silvania.copy$bin <- droplevels(silvania.copy$bin, exclude = " ")
unique(silvania.copy$bin)


# How often they show up in the camera traps
imgsPerSp <- silvania.copy %>% group_by(bin) %>% summarize(n = n()) %>% arrange(desc(n))
imgsPerSp

#plot
ggplot(imgsPerSp, aes(x = reorder(bin, -n), y = n)) + geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + xlab("Species") + ylab("Number of images")

# Build a simple species accumulation curve

# Put the data in matrix form
spMatrix <- with(silvania.copy, table(Camera.Trap.Name,bin))
spMatrix [1:10, ]

# Using function speaccum from package vegan
sp1 <- specaccum(spMatrix)
sp1
sp2 <- specaccum(spMatrix, "random")
sp2


# Plot accumulated species richness + confidence limits
plot(sp1, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="green")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

