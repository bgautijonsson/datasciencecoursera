corr <- function(directory, threshold = 0) {
  
  omit <- function(x) {
    x = na.omit(x)
    x
  }
  
  corfunc <- function(x) {
    y <- cor(x$nitrate, x$sulfate)
    y
  }
  
  data.complete = complete(directory)
  subset = subset(data.complete, nobs>= threshold)
  
  chosendata = subset$id
  
  file_names <- dir(directory)
  file_names <- paste("./", directory, "/", file_names, sep="")
  data.cor <- lapply(file_names[chosendata], read.csv)
  
  
  
  data.cor <- lapply(data.cor, omit)
  data.cor <- data.cor[sapply(data.cor, function(x) dim(x)[1]) > 0]
  data.cor
  
  cor.list <- sapply(data.cor, corfunc)
  cor.list <- unlist(cor.list)
  
  cor.list
}