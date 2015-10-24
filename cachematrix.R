## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # re-set the matrix (and clears the cache)
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # return the matrix
  get <- function() x
  # stores the inverse (cache)
  setinverse <- function(inverse) m <<- inverse
  # retrieves the value from the cache
  getinverse <- function() m
  
  # init the "object"
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  ## try to get the cached inverse
  m <- x$getinverse()
  # check if the returned value is not null (already computed)
  if(!is.null(m)) {
    # if true
    message("getting cached data")
    ## it returns the cached value
    return(m)
  }
  # otherwise it gets the matrix
  data <- x$get()
  # computes the inverse
  m <- solve(data, ...)
  # and stores (cache) it for future use
  x$setinverse(m)
  # returns the inverse
  m
}
