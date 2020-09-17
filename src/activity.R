
##---- Activity patterns ---------------------------

# Write a couple of functions for activity patterns
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

# Run it for Myrmecophaga tridactyla
actMod_Myrtri <- calcActivity(gurupi2016, "Myrmecophaga tridactyla", 2)

# Do a ciruclar plot of activity
plot.circular.activity(actMod_Myrtri[[2]], "Myrmecophaga tridactyla")

# Run it for Tamandua tetradactyla
actMod_Tamtet <- calcActivity(gurupi2016, "Tamandua tetradactyla", 2)

# Compare the activity of two species
compareAct(list(actMod_Myrtri[[1]],actMod_Tamtet[[1]]))


