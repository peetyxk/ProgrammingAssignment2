## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # initially, there's no inverse
  
  set <- function(y) {
    x <<- y        # assign new matrix to x
    inv <<- NULL   # reset cached inverse
  }
  
  get <- function() x  # returns the current matrix
  
  setinverse <- function(inverse) inv <<- inverse  # stores inverse in cache
  
  getinverse <- function() inv  # retrieves cached inverse
  
  # return a list of all 4 functions
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # retrieve cached inverse (if any)
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()        # get the matrix
  inv <- solve(data, ...)  # compute the inverse
  x$setinverse(inv)        # cache the inverse
  inv                     # return the result
}

