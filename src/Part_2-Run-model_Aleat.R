# Occupancy model for Jaguars at Parque Estadual do Rio Doce
# Juliana Pasa, Fernando Azevedo, Elildo Carvalho Jr

# Part2-Occupancy models - using the random sampling design

#the data basis used here was organized manually, and contains the following columns:Cam.Trap.Name, Photo.Date, bin, n, Start.Date, End.Date, Sampling.Period, Sampling.Unit.Name.
#which consists in respectively:name of the point (including those without records of the species), record date, species name, number of individuals (optional), start date of the sampling, end date of the sampling, year of the sampling, name of the point sampled. 

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
source(here("bin", "camera trap analysis functions-10-06-18.R"))
source(here("bin", "f-matrix-creator.R"))

#Load libraries and start from here (using the data already organized manually):

## ---- 3- Load data-------   
jaguars <- read.csv(here("data", "jaguars_ocup_aleat.csv"))

# fix dates and times
jaguars$Photo.Date <- as.Date(jaguars$Photo.Date)
jaguars$Start.Date <- as.Date(jaguars$Start.Date)
jaguars$End.Date <- as.Date(jaguars$End.Date)
#jaguars$Photo.Time <- as.POSIXct(paste(jaguars$Photo.Date, jaguars$Photo.Time))

# create a few additional columns
#jaguars$td.photo <- ymd_hms(paste(jaguars$Photo.Date, jaguars$Photo.Time, sep=" "))

# filter non-independent records (30 minute interval)
#jaguars <- f.separate.events(jaguars, 30)
#jaguars <- distinct(jaguars, Camera.Trap.Name, bin, grp, .keep_all=TRUE) # keep only independent records


## ---- 4- Create a matrix of presence/absence-------    
# Create a matrix of presence/absence of each species
# rows are camera trap points (sampling units) and columns are dates
# Use function f.matrix.creator2 to do this - cria matriz de deteccao, colapsando os dados sempre em 15 dias
paMatJAGUAR <- f.matrix.creator4(jaguars, "Panthera onca", 19) # 15 is the desired number of occasions, you can chabge it at your will 
#I changed the number of occasions to 19, because in this way each occasion lasts for almost 20 days
print(paMatJAGUAR)
View(paMatJAGUAR[["Panthera onca"]])
# Something went wrong with the matrix. First of all, it is showing us all the points, not only the ones with jaguars' records. Second, the half-bottom of the matrix is with NAs in occasions where it should be 0s or 1s (occasion 15 until the end).

## como inserir NAs "no meio" da matriz:
# nesse exemplo vamos inserir NAs na camera "Abelhas" nas ocasioes (colunas) 4, 5 e 6
#paMatJAGUAR$`Panthera onca`[1,4:6] <- NA # isto e, na linha 1 e colunas 4 a 6 substiitui o valor por NAs
# tambem da para fazer aasim: paMatJAGUAR$`Panthera onca`[1,c(4,5,6)] <- NA

#inserindo NA's nas occasions que nao foram amostradas - períodos entre a amostragem da seca e da chuva, para todos os pontos:
paMatJAGUAR$'Panthera onca'[1,8:10] <- NA  
paMatJAGUAR$'Panthera onca'[2,8:9] <- NA
paMatJAGUAR$'Panthera onca'[3,8:10] <- NA
paMatJAGUAR$'Panthera onca'[4:9,8:10] <- NA
paMatJAGUAR$'Panthera onca'[10:13,8:9] <- NA
paMatJAGUAR$'Panthera onca'[14:15,8:10] <- NA
paMatJAGUAR$'Panthera onca'[16:18,8:9] <- NA
paMatJAGUAR$'Panthera onca'[19:21,8:10] <- NA
paMatJAGUAR$'Panthera onca'[22:24,8:9] <- NA
paMatJAGUAR$'Panthera onca'[25,9] <- NA
paMatJAGUAR$'Panthera onca'[26,8:9] <- NA
paMatJAGUAR$'Panthera onca'[27,8:10] <- NA
paMatJAGUAR$'Panthera onca'[28,5:15] <- NA
paMatJAGUAR$'Panthera onca'[29,4:14] <- NA
paMatJAGUAR$'Panthera onca'[30,5:15] <- NA
paMatJAGUAR$'Panthera onca'[31:32,4:14] <- NA
paMatJAGUAR$'Panthera onca'[33,4:15] <- NA
paMatJAGUAR$'Panthera onca'[34:36,4:14] <- NA
paMatJAGUAR$'Panthera onca'[37,5:14] <- NA
paMatJAGUAR$'Panthera onca'[38,5:15] <- NA
paMatJAGUAR$'Panthera onca'[39,4:15] <- NA
paMatJAGUAR$'Panthera onca'[40:45,4:14] <- NA
paMatJAGUAR$'Panthera onca'[46,5:15] <- NA
paMatJAGUAR$'Panthera onca'[47,4:14] <- NA
paMatJAGUAR$'Panthera onca'[48,5:15] <- NA
paMatJAGUAR$'Panthera onca'[49,5:14] <- NA
paMatJAGUAR$'Panthera onca'[50:53,4:14] <- NA
paMatJAGUAR$'Panthera onca'[54,5:15] <- NA

# This creates a list were each element of the list is a presence/absence matrix for a species
summary(paMatJAGUAR)
names(paMatJAGUAR)

# Look at the matrix for Panthera onca
View(paMatJAGUAR[["Panthera onca"]])

## ---- 5- Load covariate data-------    
# Still dont have the covariates, so create dummy variables just to test the code
#Camera.Trap.Name <- rownames(paMatJAGUAR[["Panthera onca"]])
#Camera.Trap.Name <- sort(unique(jaguars$Camera.Trap.Name))
#altitude <- rnorm(length(Camera.Trap.Name)); slope <- rexp(length(Camera.Trap.Name),1/2500) # dummy variables
#covsPERD <- data.frame(Camera.Trap.Name, altitude, slope)

#Now we have the real covariates, so I muted the commands above.
covsPERD <- read.csv(here("data", "Random_covariates_occupancy.csv"))

# We just need the cameras operating in 2017. Which ones are they?
#suPERD <- unique(jaguars$Camera.Trap.Name)

# filter our covariate file - only keep the lines that matches suPERD
#covsPERD <- filter(covsPERD, Camera.Trap.Name %in% suPERD)

# Normalize the two covariates - altitude and slope
#covsPERD <- mutate(covsPERD, norm.altitude = (altitude - mean(altitude))/sd(altitude), norm.slope = (slope-mean(slope))/sd(slope))
#head(covsPERD)

# Normalize all our seven covariates. I dont know if I used the right command, because the values were kept the same... Is this correct?
covsPERD <- mutate(covsPERD, norm.rivers = (covsPERD$Dist.Rivers), norm.lakes = (covsPERD$Dist.Lakes), norm.canopy = (covsPERD$Caonpy), norm.understory = (covsPERD$Understory), norm.cities = (covsPERD$Dist.Cities), norm.euc = (covsPERD$Dist.Euc), norm.pasture = (covsPERD$Dist.Pasture))
head(covsPERD)

#Sort by camera trap unit name for analysis
covsPERD <- arrange(covsPERD, Camera.Trap.Name)
head(covsPERD)

#I ran the script until here, because my intention is to run all possible model combinations. So I muted the next commands.
# Also, I still didnt run the third script with this data. I thought it was better to correct what went wrong with the matrix first.

#-------------------------------------------------
# Inserido em 2020-07-30:
# criar dummy covariate para a ocasiao ocasiao
R <- dim(paMatJAGUAR[["Panthera onca"]])[1]
J <- dim(paMatJAGUAR[["Panthera onca"]])[2]
df <- round(rnorm(J, 2000, 50)) # valores inventados de pluviosidade para cada uma das 19 ocasioes
#dummy.obsCovs <- as.data.frame(matrix(rep(df, dim(paMatJAGUAR[["Panthera onca"]])[1]), byrow=TRUE, nrow=dim(paMatJAGUAR[["Panthera onca"]])[1], ncol=n.ocasioes))
dummy.obsCovs <- matrix(rep(df, R, byrow=TRUE), nrow=R, ncol=J)
rownames(dummy.obsCovs) <- rownames(paMatJAGUAR[["Panthera onca"]])
#dummy.obsCovs <- list(dummy.obsCovs=data.frame(dummy.obsCovs))
# no caso da pluviosidade, a covariavel de ocasiao vai ser igual para todas as cameras em cada ocasiao
# por isso basicamente vc tem 54 linhas iguais
# para um tutorial veja https://doi90.github.io/lodestar/fitting-occupancy-models-with-unmarked.html
#-------------------------------------------------


## ---- 6- Run occupancy model -------    

# Convert these matrices into a special format to analyze them in unmarked
#neste comando que está dando o erro:
umPonca <- unmarkedFrameOccu(y = paMatJAGUAR[["Panthera onca"]], siteCovs = covsPERD, obsCovs = list(dummy.obsCovs=dummy.obsCovs))
#summary(umPonca)

## fit a single season occupancy model with no covariates
#occMod0_Ponca <- occu(~1 ~1, umPonca)
# Look at the model results
#occMod0_Ponca
# Transform the estimates from log to linear
#backTransform(occMod0_Ponca, "state")
#backTransform(occMod0_Ponca, "det")

## fit model with covariate for occupancy
#occMod1_Ponca <- occu(~norm.altitude ~1, umPonca)
#occMod1_Ponca
#backTransform(occMod1_Ponca, "state")

# We cannot use backTransform for p because it is dependent on elevation
# First create a dataframe with the average value of elevation (0)
#newdata <- data.frame(norm.altitude = 0)
# Use predict to get the values
#predict(occMod1_Ponca, type="det", newdata=newdata)

# How is detection varying with elevation
#newdata <- data.frame(norm.altitude = seq(-2.8, 2, 0.05))
#mod_pred <- predict(occMod1_Ponca, type="det", newdata=newdata)
#mod_pred <- data.frame(mod_pred, elevation = newdata)

# Transform elevation back to original scale
#mod_pred <- mutate(mod_pred, elev = sd(covsPERD$altitude)*norm.altitude + mean(covsPERD$altitude))
#mod_pred

# plot it
#ggplot(mod_pred, aes(x = elev, y = Predicted)) + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, fill = "blue") + xlab("Elevation") + ylab("Detection probability")

# save as jpeg
#jpeg(here("results", "dummy_elevation_psi.jpg"), width = 800, height = 600) # Open jpeg file
#ggplot(mod_pred, aes(x = elev, y = Predicted)) + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.4, fill = "blue") + xlab("Elevation") + ylab("Detection probability")
#dev.off()

# Do another model where occupancy is a function of slope and detection is a function of altitude
#occMod2_Ponca <- occu(~norm.slope ~norm.altitude, umPonca)
#occMod2_Ponca

# Compare these models
#modelList <- fitList('psi()p()' = occMod0_Ponca, 'psi(altitude)p()' = occMod1_Ponca, 'psi(slope)p(altitude)' = occMod2_Ponca)
#modSel(modelList)

