## The two functions makeCacheMatrix and cacheSolve prevent repeated
## calculation of the inverse. After the first time the inverse is cached, and 
## future calls to cacheSolve simply retrieve the previous calculated inverse.
## Usage:   
##      cachableMatrix <- makeCacheMatrix(x = your original matrix)
##      cacheSolve(cachableMatrix, ...)


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Return a special cacheMatrix object that can cache its inverse and is 
    ## used as parameter to cacheSolve.
    ## This object also contains the four functions get, set, getinv, setinv for
    ## storing and retrieving the original matrix and its inverse.
   
  ## inv will contain the inverse matrix, until then it is set to NULL
  inv <- NULL
  ## the function set stores the original matrix in x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## the function get returns the original matrix
  get <- function () x
  ## the function setinv calculates, stores and returns the inverse
  setinv <- function(solve) inv <<- solve
  ## the function getinv returns the cached inverse
  getinv <- function() inv
  list (set = set, get = get, setinv = setinv, getinv = getinv)
}



## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache.
## It is assumed that the matrix is inversible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## first check if inverse is already calculated and cached
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  ## not cached, so we have to calculate the inverse and cache it
  message("computing inverse - may take a while")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

