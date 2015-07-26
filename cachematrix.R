## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the inverse of the matrix
## 4.get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## This function computes the inverse of the matrix
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated with no change in the matrix, then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  data <- x$get()
  invserse <- solve(data, ...)
  x$setinv(m)
  m
}
