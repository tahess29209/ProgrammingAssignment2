## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes an input matrix (assumed to be invertable) and returns
## a list of functions for setting and getting the original value and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #set the inverse to be NULL
  inv <- NULL
  #define four functions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  #return four functions as a list
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve uses the solve() function to calculate the inverse of input, X but first checks to see
## if the solution exists already. 

cacheSolve <- function(x, ...) {
  #uses the getinverse function from the list returned from makeCacheMatrix
  inv <- x$getinverse()
  #checks to see if inv has exists
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  #Solve and return the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
