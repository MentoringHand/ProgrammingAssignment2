## Put comments here that give an overall description of what your
## functions do

## This function returns a list object with four functions
## These functions are used to get/set two values. The variables used within these functions
## have a specific scope as defined in this function 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function works only if x is of type created using the above function makeCacheMatrix
## This function tries to get a value inv from the environment encapsulated within the list  
## the cache function set the inverse of the matrix to the inv variable to be cached and reused 
## when called next. So we first check if it is already set and use it as it should have been 
## set after calculation in the previous invocation.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inputmatrix <- x$get()
  inv <- solve(inputmatrix)
  x$setinverse(inv)
  inv
}
