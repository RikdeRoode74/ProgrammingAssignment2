## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inversed matrix
## - get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setinversedmatrix = setinversedmatrix,
         getinversedmatrix = getinversedmatrix)
}


## function that either calculates the inverse of a matrix or returns it's cached value when already calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinversedmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinversedmatrix(m)
  m
}
