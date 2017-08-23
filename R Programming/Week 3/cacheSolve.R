cacheSolve <- function(x, ...) {
  
  # Extract from the cache
  inverse <- x$getInverse()
  
  # If there was a value in the cache, output that value
  if(!is.null(inverse)) {
    message("Retrieving data from cache")
    return(inverse)
  }
  
  # Otherwise we extract the matrix and calculate the inverse
  data <- x$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  inverse
}