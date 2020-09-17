# Put Silvania data in the same format as Wild.ID exports

# carregar bibliotecas
library(TeachingDemos)
library(lubridate)
library(unmarked)
library(ggplot2)
library(dplyr)
library(reshape2)
library(chron)
library(vegan)
library(activity)
library(ggmap)
library(here)

# carregar esse arquivo
source(here("silvania-temporario", "camera trap analysis code-WILDID-09-20-17.R"))
source(here("silvania-temporario", "time-lag.R"))

# carregar dados
silvania <- read.csv(here("silvania-temporario", "images.csv"))
deployments <- read.csv(here("silvania-temporario", "deployments.csv"))

# change names
names(silvania) <- c("Project.Name", "Camera.Trap.Name", "image_id", "location", "is_blank", "Person.Identifying.the.Photo",
                    "wi_taxon_id", "Class", "Order", "Family", "Genus", "Species", "commom_name", "uncertainty", "timestamp", "age",
                    "sex", "animal_recognizable", "individual_id", "Number.of.Animals", "individual_animal_notes",
                    "highlighted", "color")

# new columns
silvania$ID <- seq(1:nrow(silvania))
silvania$Camera.Trap.Name <-  substr(silvania$Camera.Trap.Name, start = 10, stop = nchar(silvania$Camera.Trap.Name))
silvania$Photo.Date <- substr(silvania$timestamp, start = 1, stop = 10)
silvania$Photo.time <- substr(silvania$timestamp, start = 12, stop = nchar(silvania$timestamp))
silvania$Camera.Manufacturer <- "Bushnell"
silvania$Camera.Model <- "Tropy Cam HD"
silvania$Camera.Serial.Number <- NA
silvania$Sequence.Info <- NA
silvania$Moon.Phase <- NA
silvania$Temperature <- NA
silvania$Flash <- NA
silvania$Organization.Name <- "ICMBio/CENAP"
silvania$Sampling.Event <- 2019
silvania$Latitude <- NA
silvania$Longitude <- NA
silvania$Raw.Name <- NA
silvania$Photo.Type <- NA
silvania$Person.picking.up.the.Camera <- "Mariella Butti"
silvania$Person.setting.up.the.Camera <- "Mariella Butti"


# add latitude and longitude and start and end dates
df1 <- distinct(deployments, placename, latitude, longitude, start_date, end_date)
names(df1) <- c("Camera.Trap.Name", "Longitude", "Latitude", "Camera.Start.Date", "Camera.End.Date")
df1$Camera.Trap.Name <- as.factor(df1$Camera.Trap.Name)

# add unify coordinates for all sites-years in the dataset
df2 <- silvania
newcols <- c("Latitude", "Longitude", "Camera.Start.Date", "Camera.End.Date")
df2[newcols] <- lapply(newcols, function(x) df1[[x]][match(df2$Camera.Trap.Name, df1$Camera.Trap.Name)])
#View(df2)
silvania <- df2

# remove some columns
names(silvania)
silvania$image_id <- NULL
silvania$location <- NULL
silvania$is_blank <- NULL
silvania$image_id <- NULL
silvania$wi_taxon_id <- NULL
silvania$uncertainty <- NULL
silvania$timestamp <- NULL
silvania$age <- NULL
silvania$sex <- NULL
silvania$animal_recognizable <- NULL
silvania$individual_id <- NULL
silvania$individual_animal_notes <- NULL
silvania$commom_name <- NULL
silvania$highlighted <- NULL
silvania$color <- NULL
silvania$licence <- NULL

# change higher taxonomic levels to uppercase
silvania$Class <- toupper(silvania$Class)
silvania$Class[silvania$Class == "NO CV RESULT"] <- NA
silvania$Class[silvania$Class == "UNKNOWN"] <- NA
silvania$Order <- toupper(silvania$Order)
silvania$Order[silvania$Order == "NO CV RESULT"] <- NA
silvania$Order[silvania$Order == "UNKNOWN"] <- NA
silvania$Family <- toupper(silvania$Family)
silvania$Family[silvania$Family == "NO CV RESULT"] <- NA
silvania$Family[silvania$Family == "UNKNOWN"] <- NA

# reorder columns
col_order <- c("ID",	"Project.Name",	"Camera.Trap.Name",	"Latitude",	"Longitude",	
               "Sampling.Event",	"Photo.Type",	"Photo.Date",	"Photo.time",	"Raw.Name",	
               "Class",	"Order",	"Family",	"Genus",	"Species",	"Number.of.Animals",	
               "Person.Identifying.the.Photo",	"Camera.Serial.Number",	"Camera.Start.Date",	"Camera.End.Date",	"Person.setting.up.the.Camera",	"Person.picking.up.the.Camera",	"Camera.Manufacturer",	"Camera.Model",	"Sequence.Info",	"Moon.Phase",	"Temperature",	"Flash",	"Organization.Name")
silvania <- silvania[,col_order]

# check dates
sort(unique((silvania$Camera.Start.Date)))
sort(unique((silvania$Camera.End.Date)))
sort(unique((silvania$Photo.Date)))

# check timelag
silvania$Camera.Start.Date <- as.Date(silvania$Camera.Start.Date)
silvania$Camera.End.Date <- as.Date(silvania$Camera.End.Date)
silvania$Photo.Date <- as.Date(silvania$Photo.Date)
time.lag(silvania) # everything ok, no need to fix start or end dates

# save as csv
write.csv(silvania, here("silvania-temporario", "Wild_ID_silvania.csv"), row.names = FALSE)


