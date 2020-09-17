# Occupancy model for Xenarthrans at Gurupi Biological Reserve
# Elildo Carvalho Jr, Alexandre Martins, Eloisa Mendonca

# Part2-Occupancy models

# Read in TEAM data set and create and format so it is ready for wildlife community model
# Written by Jorge Ahumada @ Conservation International
# Adapted by Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

##----- 1 - Load libraries-----
library(dplyr)
library(lubridate)
library(reshape2)
library(ggplot2)
library(unmarked)
library(here)


##----- 2 - Source files-----
source(here("bin", "camera trap analysis functions-10-06-18.R")) # using package here to build a path to the subdirectory "bin"
source(here("bin", "f-matrix-creator-experimental-probably-ok-but-need-check.R"))

## ---- 3- Load data-------
gurupi2016 <- read.csv(here("data", "gurupi2016.csv"))

# fix dates and times
gurupi2016$Photo.Date <- as.Date(gurupi2016$Photo.Date)
gurupi2016$Camera.Start.Date <- as.Date(gurupi2016$Camera.Start.Date)
gurupi2016$Camera.End.Date <- as.Date(gurupi2016$Camera.End.Date)
#gurupi2016$Photo.Time <- as.POSIXct(paste(gurupi2016$Photo.Date, gurupi2016$Photo.Time))

# create a few additional columns
gurupi2016$bin <- paste(gurupi2016$Genus,gurupi2016$Species, sep=" ")
gurupi2016$td.photo <- ymd_hms(paste(gurupi2016$Photo.Date, gurupi2016$Photo.Time, sep=" "))
gurupi2016$Start.Date <- gurupi2016$Camera.Start.Date
gurupi2016$End.Date <- gurupi2016$Camera.End.Date
gurupi2016$Sampling.Period <- gurupi2016$Sampling.Event
gurupi2016$Sampling.Unit.Name <- gurupi2016$Camera.Trap.Name

# filter non-independent records (30 minute interval)
gurupi2016 <- f.separate.events(gurupi2016, 30)
gurupi2016 <- distinct(gurupi2016, Camera.Trap.Name, bin, grp, .keep_all=TRUE) # keep only independent records

## ---- 3- Load covariate data-------
# Still dont have the covariates, so create dummy variaables just to test the code
Camera.Trap.Name <- sort(unique(gurupi2016$Camera.Trap.Name))
altitude <- rnorm(length(Camera.Trap.Name)); slope <- rexp(length(Camera.Trap.Name),1/2500) # dummy variables
covsRBG <- data.frame(Camera.Trap.Name, altitude, slope)

# We just need the cameras operating in 2016. Which ones are they?
suRBG <- unique(gurupi2016$Camera.Trap.Name)

# filter our covariate file
covsRBG <- filter(covsRBG, Camera.Trap.Name %in% suRBG)

# Normalize the two covariates - altitude and slope
covsRBG <- mutate(covsRBG, norm.altitude = (altitude - mean(altitude))/sd(altitude), norm.slope = (slope-mean(slope))/sd(slope))
head(covsRBG)

#Sort by camera trap unit name for analysis
covsRBG <- arrange(covsRBG, Camera.Trap.Name)
head(covsRBG)

# Create a matrix of presence/absence of each species
# rows are camera trap points (sampling units) and columns are dates
# Use function f.matrix.creator2 to do this
#paMatsGurupi <- f.matrix.creator2(gurupi2016)
paMatsGurupi <- f.matrix.creator4(gurupi2016, cams, species, 15)

# This creates a list were each element of the list is a presence/absence matrix for a species
summary(paMatsGurupi)
names(paMatsGurupi)

# Look at the matrix for Myrmecophaga tridactyla
View(paMatsGurupi[["Myrmecophaga tridactyla"]])

# Convert these matrices into a special format to analyze them in unmarked
umGurupi_Myrtri <- unmarkedFrameOccu(y = paMatsGurupi[["Myrmecophaga tridactyla"]], siteCovs = covsRBG)
summary(umGurupi_Myrtri)

## fit a single season occupancy model with no covariates
occMod0_Myrtri <- occu(~1 ~1, umGurupi_Myrtri)
# Look at the model results
occMod0_Myrtri
# Transform the estimates from log to linear
backTransform(occMod0_Myrtri, "state")
backTransform(occMod0_Myrtri, "det")

## fit model with covariate for occupancy
occMod1_Myrtri <- occu(~norm.altitude ~1, umGurupi_Myrtri)
occMod1_Myrtri
backTransform(occMod1_Myrtri, "state")

# We cannot use backTransform for p because it is dependent on elevation
# First create a dataframe with the average value of elevation (0)
newdata <- data.frame(norm.altitude = 0)
# Use predict to get the values
predict(occMod1_Myrtri, type="det", newdata=newdata)

# How is detection varying with elevation
newdata <- data.frame(norm.altitude = seq(-2.8, 2, 0.05))
mod_pred <- predict(occMod1_Myrtri, type="det", newdata=newdata)
mod_pred <- data.frame(mod_pred, elevation = newdata)

# Transform elevation back to original scale
mod_pred <- mutate(mod_pred, elev = sd(covsRBG$altitude)*norm.altitude + mean(covsRBG$altitude))
mod_pred

# plot it
ggplot(mod_pred, aes(x = elev, y = Predicted)) + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, fill = "blue") + xlab("Elevation") + ylab("Detection probability")

# save as jpeg
jpeg(here("results", "dummy_elevation_psi.jpg"), width = 800, height = 600) # Open jpeg file
ggplot(mod_pred, aes(x = elev, y = Predicted)) + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, fill = "blue") + xlab("Elevation") + ylab("Detection probability")
dev.off()

# Do another model where occupancy is a function of slope and detection is a function of altitude
occMod2_Myrtri <- occu(~norm.slope ~norm.altitude, umGurupi_Myrtri)
occMod2_Myrtri

# Compare these models
modelList <- fitList('psi()p()' = occMod0_Myrtri, 'psi(altitude)p()' = occMod1_Myrtri, 'psi(slope)p(altitude)' = occMod2_Myrtri)
modSel(modelList)

