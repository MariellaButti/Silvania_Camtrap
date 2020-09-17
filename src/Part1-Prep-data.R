# Part1-Prepare data

# Read in a TEAM data set and create and format so it is ready for wildlife community model
# Written by Jorge Ahumada @ Conservation International
# Adapted by Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-02

#----- 1 - Load libraries-----
library(dplyr)
library(lubridate)
library(here)


#----- 2 - Source files-----
here <- here::here # to avoid confusion with "here" function from lubridate
#source(here("bin", "camera trap analysis functions-10-06-18.R")) # using package here to build a path to the subdirectory "bin"
source(here("bin", "ahumada_codes.R"))
source(here("bin", "f-matrix-creator-experimental-probably-ok-but-need-check.R"))
source(here("src", "fix_species_names.R")) # fix some names and remove "false" species


## ----Load data-------
#dataRBG <- f.readin.fix.data(here("data", "Wild_ID_RBG_2016to2019.csv"))
dataRBG <- read.csv(here("data", "Wild_ID_RBG_2016to2019.csv"))

# some fixes
dataRBG$Sampling.Unit.Name <- as.factor(dataRBG$Camera.Trap.Name)
colnames(dataRBG)[9] <- "Photo.Time"
dataRBG$bin <- factor(dataRBG$bin)

# fix date formats (only needed if data was read with read.csv instead of f.readin.fix.data)
dataRBG$Photo.Date <- as.Date(dataRBG$Photo.Date)
dataRBG$Camera.Start.Date <- as.Date(dataRBG$Camera.Start.Date)
dataRBG$Camera.End.Date <- as.Date(dataRBG$Camera.End.Date)
dataRBG$Start.Date <- as.Date(dataRBG$Start.Date)
dataRBG$End.Date <- as.Date(dataRBG$End.Date)


# fix species names
f.fix.species.names(dataRBG)
dataRBG <- dataTemp # use new df created by function

#----- 4 - Extract binary presence/absence matrices for each species
species <- unique(dataRBG$bin)
cams <- unique(dataRBG$Camera.Trap.Name)
years <- unique(dataRBG$Sampling.Event)
secondPeriods <- 1:15

# Separate different years - clunky?
#dataRBG2016 <- dplyr::filter(dataRBG, Sampling.Event == 2016)
dataRBG2017 <- dplyr::filter(dataRBG, Sampling.Event == 2017)
#dataRBG2018 <- dplyr::filter(dataRBG, Sampling.Event == 2018)
#dataRBG2019 <- dplyr::filter(dataRBG, Sampling.Event == 2019)

# use only first 55 days of sampling
# since we are using only 1st 55 days, we must reset end dates using max photo date
f.update.end.data <- function(data, duration){
  new.end.date <- min(data$Start.Date)+duration
  df1 <- subset(data, Photo.Date <= new.end.date)
  for(i in 1:nrow(df1)){ 
    if (df1$End.Date[i] > new.end.date) {
      df1$End.Date[i] <- new.end.date
    }
  }
  df1$Camera.Start.Date <- df1$Start.Date
  df1$Camera.End.Date <- df1$End.Date
  assign("df1", df1, envir=.GlobalEnv)
} # End of function

#f.update.end.data(dataRBG2016, 55)
#dataRBG2016 <- df1
#f.update.end.data(dataRBG2017, 55)
#dataRBG2017 <- df1
#f.update.end.data(dataRBG2018, 55)
#dataRBG2018 <- df1
#f.update.end.data(dataRBG2019, 55)
#dataRBG2019 <- df1



# Create presence/absence matrices for each species each year
# matrix dimensions are all identical accross species and years

#paMats2016 <- f.matrix.creator3(dataRBG2016, cams, species)
#paMats2017 <- f.matrix.creator3(dataRBG2017, cams, species)
#paMats2018 <- f.matrix.creator3(dataRBG2018, cams, species)
#paMats2019 <- f.matrix.creator3(dataRBG2019, cams, species)

# before using f.matrix.creator check sampling duration
duration <- function(data) {
  sampling.days <- max(data$End.Date) - min(data$Start.Date) + 1
  return(sampling.days)
}

#duration(dataRBG2016) # 55 days ok
duration(dataRBG2017) 
#duration(dataRBG2018) 
#duration(dataRBG2019) 

#paMats2016 <- f.matrix.creator4(dataRBG2016, species, 11)
paMats2017 <- f.matrix.creator4(dataRBG2017, species, 15) # 79 days duration, 15.8 5-day occasions
#paMats2018 <- f.matrix.creator4(dataRBG2018, species, 11)
#paMats2019 <- f.matrix.creator4(dataRBG2019, species, 11)

# check species names
names(paMats2017)

# Myrmecophaga is the 8th species
Mtridactyla <- as.data.frame(paMats2017[8]); names(Mtridactyla) <- seq(1:ncol(Mtridactyla))

# Priodontes maximus is the 21th species
Pmaximus <- as.data.frame(paMats2017[21]); names(Pmaximus) <- seq(1:ncol(Pmaximus))

# function to create species data
createSppData <- function(x) {
  for(i in 1:length(x)){
    df1 <- as.data.frame(paMats2016[x])
    colnames(df1) <- seq(1:length(colnames(df1))); colnames(df1) <- paste("X2016.", colnames(df1), sep="")
    df2 <- as.data.frame(paMats2017[x]) 
    colnames(df2) <- seq(1:length(colnames(df2))); colnames(df2) <- paste("X2017.", colnames(df2), sep="")
    df3 <- as.data.frame(paMats2018[x]) 
    colnames(df3) <- seq(1:length(colnames(df3))); colnames(df3) <- paste("X2018.", colnames(df3), sep="")
    df4 <- as.data.frame(paMats2019[x]) 
    colnames(df4) <- seq(1:length(colnames(df4))); colnames(df4) <- paste("X2019.", colnames(df4), sep="")
    bla <- cbind(df1, df2, df3, df4)
  }
  assign(paste("dataRBG_species", gsub(" ", "_", x), sep="_"), bla, envir = .GlobalEnv)
}

# check if it works
#createSppData("Psophia obscura")
#dataRBG_species_Psophia_obscura


#----- 4 - Read covariate data

# Land cover Mapbiomas
cover <- read.csv(here("data", "cover_mapbiomas.csv"))
names(cover)[2] <- "Camera.Trap.Name"
#names(cover)[9] <- "landCover.500m.17"
cover$Camera.Trap.Name <- gsub("Ctrbg", "CT-RBG-", cover$Camera.Trap.Name)
cover$Camera.Trap.Name <- gsub("Ctrgb", "CT-RBG-", cover$Camera.Trap.Name)

# Distance to water
dist.water <- read.csv(here("data", "dist_agua.csv"))
names(dist.water)[3] <- "Camera.Trap.Name"
names(dist.water)[4] <- "dist.water"
dist.water$Camera.Trap.Name <- gsub("Ctrbg", "CT-RBG-", dist.water$Camera.Trap.Name)
dist.water$Camera.Trap.Name <- gsub("Ctrgb", "CT-RBG-", dist.water$Camera.Trap.Name)

# Distance to forest edge
dist.edge <- read.csv(here("data", "dist_to_edge.csv"))
names(dist.edge) <- c("Camera.Trap.Name", "dis.to.edge")

# Slope
slope.elev <- read.csv(here("data", "slope_altitd_pt_cam.csv"))
#names(slope.elev)[2] <- "Camera.Trap.Name"
names(slope.elev) <- c("seq", "Camera.Trap.Name", "slope", "elevation")
slope.elev$Camera.Trap.Name <- gsub("Ctrbg", "CT-RBG-", slope.elev$Camera.Trap.Name)
slope.elev$Camera.Trap.Name <- gsub("Ctrgb", "CT-RBG-", slope.elev$Camera.Trap.Name)

# tree structure
trees <- read.csv(here("data", "tree_structure.csv"))


## create a single covariates dataframe
covars <- merge(cover[,c(2,7)], dist.water[,3:4], by="Camera.Trap.Name")
covars <- merge(covars, dist.edge[,1:2], by="Camera.Trap.Name")
covars <- merge(covars, slope.elev[,2:4], by="Camera.Trap.Name")
covars <- merge(covars, trees[,c(2,4,6,7)], by="Camera.Trap.Name")

# merge 
Mtridactyla$Camera.Trap.Name <- rownames(Mtridactyla)
Mtridactyla <- merge(Mtridactyla, covars, by="Camera.Trap.Name")

Pmaximus$Camera.Trap.Name <- rownames(Pmaximus)
Pmaximus <- merge(Pmaximus, covars, by="Camera.Trap.Name")

# Save to disk
saveRDS(Mtridactyla, here("data","Mtridactyla.rds"))
saveRDS(Pmaximus, here("data","Pmaximus.rds"))
