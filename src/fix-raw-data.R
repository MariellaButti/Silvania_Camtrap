# Occupancy model for Xenarthrans at Gurupi Biological Reserve
# Elildo Carvalho Jr, Alexandre Martins, Eloisa Mendonca

# Part1-Prepare data

# Read in TEAM data set and create and format so it is ready for wildlife community model
# Written by Jorge Ahumada @ Conservation International
# Adapted by Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

##----- 1 - Load libraries-----
library(dplyr)
library(lubridate)
library(here)


##----- 2 - Source files-----
source(here("bin", "camera trap analysis functions-10-06-18.R")) # using package here to build a path to the subdirectory "bin"


## ----Load data-------
gurupi2016 <- read.csv(here("data", "Wild_ID_Gurupi_2016.csv"))

# some small fixes
gurupi2016$Sampling.Event <- 2016
gurupi2016$Organization.Name <- "CENAP"

# fix dates
gurupi2016$Photo.Date <- as.Date(gurupi2016$Photo.Date, format="%d/%m/%Y")
gurupi2016$Camera.Start.Date <- as.Date(gurupi2016$Camera.Start.Date, format="%d/%m/%Y")
gurupi2016$Camera.End.Date <- as.Date(gurupi2016$Camera.End.Date, format="%d/%m/%Y")

# check dates
min(gurupi2016$Camera.Start.Date); max(gurupi2016$Camera.End.Date)
min(gurupi2016$Photo.Date); max(gurupi2016$Photo.Date)
min(gurupi2016$Photo.time); max(gurupi2016$Photo.time)

# check the time lag between start date and 1st photo, last photo and end date
# if lag is too large there is still something wrong in dates, fix by redefining start and end dates 
time.lag <- function(data){
  df <- data.frame(matrix(NA, nrow = length(unique(data$Camera.Trap.Name)), ncol = 3))
  names(df) <- c("Camera.Trap.Name", "diff.start", "diff.end")
  df$Camera.Trap.Name <- unique(data$Camera.Trap.Name)
  
  for(i in 1:nrow(df)){
    df1 <- subset(data, Camera.Trap.Name == df[i,1])
    start <- as.Date(min(df1$Camera.Start.Date))
    end <- as.Date(max(df1$Camera.End.Date))
    min.photo <- as.Date(min(df1$Photo.Date))
    max.photo <- as.Date(max(df1$Photo.Date))
    df[i,2] <- min.photo-start
    df[i,3] <- end-max.photo
  }
  
  return(df) # check
}
time.lag(gurupi2016)

# fix wrong end date
gurupi2016[gurupi2016$Camera.Trap.Name=="CT-RBG-2-73",]$Camera.End.Date <- max(subset(gurupi2016, Camera.Trap.Name=="CT-RBG-2-73")$Photo.Date)

# write csv
write.csv(gurupi2016, here("data", "gurupi2016.csv"), row.names = FALSE)

