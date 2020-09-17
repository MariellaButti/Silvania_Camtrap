# function to calculate the time lag between start date and first photo, and between last photo and end date
# if time lag is negative or too large there is something wrong in the data
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

