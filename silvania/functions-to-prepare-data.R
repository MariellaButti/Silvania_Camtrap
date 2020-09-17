
# Carvalho Jr functions for creating detection histories
# Adapted from Jorge Ahumada functions
# f.matrix.creator.elildo creates detection histpories collapsing 7-days into one occasion
# 

# for testing:
#data <- jamari

#--------------

f.shrink.matrix.elildo <- function(matrix){
  nc<-dim(matrix)[2] 
  #if(!nc%%(nc/10)) { # if the number of columns is exactly divisible by nc/10, ELILDO NOTE: I am collapsing 10-days to 1 occasion, thus nc/10, for other occ lengths just change the denominator
  newc<-nc%/%(nc/10)
  old.cols<-seq(1,nc,newc)
  new.matrix<-matrix(NA,nr=nrow(matrix),nc=(nc/10))
  for(i in 1:ncol(new.matrix)){
    new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T) # collapses several days into one and fill column in new.matrix 
  }
  #} else{
  #    rem<-nc%%(round(nc/10, 0))
  #    newc<-nc%/%(nc/10)
  #    old.cols<-seq(1,nc-rem,newc)
  #    new.matrix<-matrix(NA,nr=nrow(matrix),nc=(nc/10))
  #    for(i in 1:ncol(new.matrix)-1) {
  #for(i in 1:ncol(new.matrix)) {
  #    new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
  #    new.matrix[,ncol(new.matrix)]<-apply(matrix[,old.cols[ncol(new.matrix)]:nc],1,max,na.rm=T) 
  #  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}

#--------------

f.matrix.creator.elildo <- function(data){
  require(dplyr)
  require(lubridate)
  #results object
  res<-list()
  
  #get the dimensions of the matrix
  
  #list if sanpling units
  data$Sampling.Unit.Name <- data$Camera.Trap.Name
  cams <- unique(data$Sampling.Unit.Name)
  cams <- sort(cams)
  rows <- length(cams)
  #species <- unique(data$bin)
  #start and end dates of sampling periods
  #data <- dplyr::filter(data, Sampling.Period == year)
  min <- min(data$Start.Date)
  max <- max(data$End.Date)
  cols <- max - min + 1
  
  #sampling period
  date.header <- seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$Start.Date),data$Sampling.Unit.Name,unique)
  #start.dates <- start.dates[start.dates != ""]
  #start.dates <- start.dates[!is.na(start.dates)]
  nms<-names(start.dates)
  start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  
  end.dates<-tapply(as.character(data$End.Date),data$Sampling.Unit.Name,unique)
  #end.dates <- end.dates[end.dates != ""]
  #end.dates <- end.dates[!is.na(end.dates)]
  end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==start.dates[j])
    hi<-which(date.header==end.dates[j])
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[j,indx]<-0
    } else next
  }
  mat.template<-mat
  #get the species
  species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$bin==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$Photo.Date[indx]
    cameras<-data$Sampling.Unit.Name[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==dates.cameras[j,1])
      row<-which(cams==dates.cameras[j,2])
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    # reduce the size of the matrix
    mat <- f.shrink.matrix.elildo(mat)
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}

# Carvalho Jr functions for creating detection histories
# Adapted from Jorge Ahumada functions
# f.matrix.creator.elildo creates detection histpories collapsing 7-days into one occasion
# 

# for testing:
#data <- jamari

#--------------

f.shrink.matrix.elildo <- function(matrix){
  nc<-dim(matrix)[2] 
  #if(!nc%%(nc/10)) { # if the number of columns is exactly divisible by nc/10, ELILDO NOTE: I am collapsing 10-days to 1 occasion, thus nc/10, for other occ lengths just change the denominator
  newc<-nc%/%(nc/10)
  old.cols<-seq(1,nc,newc)
  new.matrix<-matrix(NA,nr=nrow(matrix),nc=(nc/10))
  for(i in 1:ncol(new.matrix)){
    new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T) # collapses several days into one and fill column in new.matrix 
  }
  #} else{
  #    rem<-nc%%(round(nc/10, 0))
  #    newc<-nc%/%(nc/10)
  #    old.cols<-seq(1,nc-rem,newc)
  #    new.matrix<-matrix(NA,nr=nrow(matrix),nc=(nc/10))
  #    for(i in 1:ncol(new.matrix)-1) {
  #for(i in 1:ncol(new.matrix)) {
  #    new.matrix[,i]<-apply(matrix[,old.cols[i]:(old.cols[i]+newc-1)],1,max,na.rm=T)
  #    new.matrix[,ncol(new.matrix)]<-apply(matrix[,old.cols[ncol(new.matrix)]:nc],1,max,na.rm=T) 
  #  }
  new.matrix[new.matrix=="-Inf"]<-NA
  rownames(new.matrix)<-rownames(matrix)
  new.matrix
}

#--------------

f.matrix.creator.elildo <- function(data){
  require(dplyr)
  require(lubridate)
  #results object
  res<-list()
  
  #get the dimensions of the matrix
  
  #list if sanpling units
  data$Sampling.Unit.Name <- data$Camera.Trap.Name
  cams <- unique(data$Sampling.Unit.Name)
  cams <- sort(cams)
  rows <- length(cams)
  #species <- unique(data$bin)
  #start and end dates of sampling periods
  #data <- dplyr::filter(data, Sampling.Period == year)
  min <- min(data$Start.Date)
  max <- max(data$End.Date)
  cols <- max - min + 1
  
  #sampling period
  date.header <- seq(from=min,to=max, by="days")
  mat<-matrix(NA,rows,cols,dimnames=list(cams,as.character(date.header)))
  
  #for all cameras, determine the open and close date and mark in the matrix
  start.dates<-tapply(as.character(data$Start.Date),data$Sampling.Unit.Name,unique)
  #start.dates <- start.dates[start.dates != ""]
  #start.dates <- start.dates[!is.na(start.dates)]
  nms<-names(start.dates)
  start.dates<-ymd(start.dates)
  names(start.dates)<-nms
  
  end.dates<-tapply(as.character(data$End.Date),data$Sampling.Unit.Name,unique)
  #end.dates <- end.dates[end.dates != ""]
  #end.dates <- end.dates[!is.na(end.dates)]
  end.dates<-ymd(end.dates)
  names(end.dates)<-nms
  
  #outline the sampling periods for each camera j
  for(j in 1:length(start.dates)){
    #for each camera beginning and end of sampling
    low<-which(date.header==start.dates[j])
    hi<-which(date.header==end.dates[j])
    if(length(low)+length(hi)>0){
      indx<-seq(from=low,to=hi)
      mat[j,indx]<-0
    } else next
  }
  mat.template<-mat
  #get the species
  species<-unique(data$bin)
  #construct the matrix for each species i
  for(i in 1:length(species)){
    indx<-which(data$bin==species[i])
    #dates and cameras when/where the species was photographed
    dates<-data$Photo.Date[indx]
    cameras<-data$Sampling.Unit.Name[indx]
    dates.cameras<-data.frame(dates,cameras)
    #unique combination of dates and cameras 
    dates.cameras<-unique(dates.cameras)
    #fill in the matrix
    for(j in 1:length(dates.cameras[,1])){
      col<-which(date.header==dates.cameras[j,1])
      row<-which(cams==dates.cameras[j,2])
      mat[row,col]<-1
    }
    mat.nas<-is.na(mat)
    sum.nas<-apply(mat.nas,2,sum)
    indx.nas<-which(sum.nas==rows)
    if(length(indx.nas)>0){
      mat<-mat[,-indx.nas]
    }
    # reduce the size of the matrix
    mat <- f.shrink.matrix.elildo(mat)
    res<-c(res,list(mat))
    #return the matrix to its original form
    mat<-mat.template
  }
  
  names(res)<-species
  #res<-lapply(res,f.dum)
  res
  
}
