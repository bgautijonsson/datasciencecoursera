pollutantmean <- function(directory, pollutant, id=1:332) {
  
  ## Directory is a character vector of length 1 indicating
  ## the location of the csv file
  ## 'id' is an integer vector indicating the monitor id numbers
  ## to be used
  file_names <- dir(directory)
  file_names <- paste("./", directory, "/", file_names, sep="")
  data <- do.call(rbind, lapply(file_names[id], read.csv))
  ## 'pollutant' is a character vector of lenght 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either sulfate or nitrate
  pollutant.mean <- colMeans(data[pollutant], na.rm=TRUE)
  ## Return the mean of the pollutant across all monitors listed
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  pollutant.mean
}