setwd('/users/notandi/data science specialization/r programming/week 3/programming assignment')

source('makeCacheMatrix.R')
source('cacheSolve.R')

# Making the chache matrix
a <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow=2, ncol=))

# Checking that it's a matrix
a$getMatrix()

# The first time we call cacheSolve, it computes the inverse
cacheSolve(a)
# The second time, we get the cached inverse
cacheSolve(a)

# When we change the matrix the cache is emptied
a$setMatrix(matrix(c(3, 4, 5, 6), nrow=2, ncol=2))

# Now we compute a new inverse and cache it again
cacheSolve(a)
cacheSolve(a)
a$getInverse()

