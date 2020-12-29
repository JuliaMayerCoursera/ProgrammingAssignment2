
## The makeCacheMatrix function creates a special "cache matrix" (or, if applied
## to an ordinary matrix, converts it into a "cache matrix"). The cache matrix
## is a list which contains the following four functions:
##
##  set: sets the value of the matrix (which is stored as a local variable)
##  get: retrieve the internally stored value of the matrix
##  setInverse: sets the value of the internally stored inverse of the matrix
##  getInverse: retrieve the value of the inverse of the matrix

makeCacheMatrix <- function(sample = matrix()) {
  invsample <- NULL
  set <- function(x) {
    Sample <<- x
    invsample <<- NULL
  }
  get <- function() sample
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() invsample
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is applied to a cacheMatrix and returns a matrix that is the 
## inverse of the internally stored matric in the cacheMatrix.
## cacheSolve works by retrieving the cached inverse in the cacheMatrix.
## If this is not null, it means that the inverse has previously been calculated
## and stored in the cache. A message is displayed and the stored inverse is returned. 
## If the inverse has not already been calculated, solve() is used to calculate 
## it, and it is stored in the cacheMatrix object as well as being returned.

cacheSolve <- function(sample, ...) {
  ## Return a matrix that is the inverse of 'sample'
  inv <- sample$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(invsample)
  }
  mat <- sample$get()
  invsample <- solve(mat, ...)
  sample$setInverse(invsample)
  invsample
}
