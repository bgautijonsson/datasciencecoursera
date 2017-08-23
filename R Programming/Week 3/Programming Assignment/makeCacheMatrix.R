makeCacheMatrix <- function(x = matrix()) {
  
  # Initially NULL since there is no cached inverse
  inverse <- NULL
  
  # Function to create or change the matrix
  setMatrix <- function(newValue) {
    x <<- newValue
    # Since the matrix changed we empty the cache
    inverse <<- NULL
  }
  
  # Returns the matrix
  getMatrix <- function() {
    x
  }
  
  # Caches the given inverse solution
  cacheInverse <- function(solution) {
    inverse <<- solution
  }
  
  # Get the cached inverse
  getInverse <- function() {
    inverse
  }
  
  # Returns all the funcitons as a list
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}