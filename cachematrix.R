## These functions are used to reduce the costly computation
## of repeatedly calculating the inverse of a matrix by
## placing the inverse in a cache.

## This function is used to set and get the cached matrix and inverse matrix 
## from the cache.

makeCacheMatrix <- function(x = matrix()) {
  # initialize cached matrix
  m <- NULL
  
  # set cached matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get cached matrix
  get <- function() return(x)
  
  # set cached inverse matrix
  setInverse <- function(solve) m <<- solve
  
  # get cached inverse matrix
  getInverse <- function() m
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is used to solve the inverse of the matrix
## or get the cached inverse if it has been previously cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  # check if cache inverse exists 
  # get cache inverse if exists
  # calculate cache inverse if not
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } else {
    message("calculating inverse")
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
