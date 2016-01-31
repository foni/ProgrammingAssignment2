## Write a function similar to base.solve() that computes the inverse of a matrix, 
## but caches the result on the first call so that for subsequent calls on the same matrix, 
## it returns the cached result instead of computing the inverse again.
## This is implemented by two functions: 
## "makeCacheMatrix()" - creates a "cachable" matrix object given a matrix object.
## "cacheSolve()" - computes the inverse of a "cachable" matrix object.


## makeCacheMatrix():
## given a matrix object "x", creates a "cachable" matrix object by 
## returning a list of the following functions:
## get() : 
##      Returns the currently set matrix. 
##      When created first time by a call makeCacheMatrix(x), it returns "x"
## set() :
##      Sets current matrix
## getinverse() :
##      Gets the inverse of the current matrix
## setinverse() :
##      Sets the inverse of the current matrix.
##      This is useful when you have a matrix and its inverse already computed
##      and you want to create a "cacheable" matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # set() function
  set <- function(y) {
    x <<- y # set the "current" matrix using <<- instead of <-
    inverse <<- NULL # reset the inverse to NULL. Again use <<- instead of <-
  }
  
  # get() function
  get <- function() x
  
  # setinverse() function
  setinverse <- function(inv) inverse <<- inv
  
  # getinverse() function
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve():
## given a "cachable" matrix object "x" created by a call to makeCacheMatrix(),
## return the inverse if it already cached, otherwise it uses base.solve() to
## compute the inverse of the "current" matrix of x, caches it and then returns
## the inverse
cacheSolve <- function(x, ...) {
  # get the cached inverse
  inverse <- x$getinverse()
  
  # return if the cached inverse is not NULL
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # get the "current" matrix
  data <- x$get()
  
  # compute the inverse using base.solve()
  inverse <- solve(data, ...)
  
  # cache the inverse
  x$setinverse(inverse)
  
  # return the inverse
  inverse
}
