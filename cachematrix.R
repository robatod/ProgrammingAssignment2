#
# 
# R Programming - Week 3
# Author: Robert DeWitt
# Date: March 13, 2016
#
# A series of functions which generate a matrix and cache it's inverse, 
# subsequent attempts to create the same inverse will pull the inverse from the 
# cache.
# 


#
# Function: makeCacheMatrix
# set up wrapper around the matrix to allow for functions to be called on the 
# matrix and stored globally
#
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#
# Function: cacheSolve
# Return a matrix that is the inverse of 'x', cache the inverse so it can be retrieved later
# 
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

