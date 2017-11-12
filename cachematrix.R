## The following functions create a special object that stores
## an invertible square matrix and cache's its inverse.


## This function creates a special "matrix" object that is a
## a list containing the functions to 
## 1.   set the value of the vector
## 2.   get the value of the vector
## 3.   set the value of the mean
## 4.   get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special
## "matrix" created with the above function. However, it first
## checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via
## the setinv function.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}
