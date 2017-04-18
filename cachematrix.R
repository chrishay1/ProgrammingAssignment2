##This script creates 2 functions; makeCacheMatrix and cacheSolve
##makeCacheMatrix(X) caches the inverse of a matrix X (using the solve function)
##cachesolve then looks to see if the inverse has already been calculated. 
##If so, it gets the inverse from the cache, and calculates it if not

##create the makeCacheMatrix function


makeCacheMatrix <- function(X = numeric()) {
  m <- NULL
  set <- function(y) {
    X <<- y
    m <<- NULL
  }
  get <- function() X
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

##create the cacheSolve function

cacheSolve <- function(X, ...) {
  m <- X$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- X$get()
  m <- solve(data, ...)
  X$setinverse(m)
  m
}

