complete <- function(directory, id=1:332) {
  # find the files by pasting the name and lapply looping them to daa
  directory <- 'specdata'
  file_names <- dir(directory)
  file_names <- paste("./", directory, "/", file_names, sep="")
  data <- lapply(file_names[id], read.csv)
  
  # make a list of the number of complete cases
  selected <- lapply(data, complete.cases)
  selected <- lapply(selected, sum)
  
  # convert the list to a dataframe
  numcomplete <- do.call(rbind.data.frame, selected)
  
  # add a column listing which id is in the row
  colnames(numcomplete) <- c('nobs')
  numcomplete <- cbind (id, numcomplete)
  numcomplete
  
}


