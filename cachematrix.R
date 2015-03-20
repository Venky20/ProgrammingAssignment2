## This is for Programming Assignment 2 to cache the inverse of the matrix
## Objective is to understand the benefits of caching and 'Lexical Scopting'
## Caching will help us to avoid the time consuming calculations again and again, 
##   instead it can be directly accessed from memory  if it is cached. 

## The following 'makeCacheMatrix' function creates a "matrix" object that can cache its inverse.
#1.  set the value of the matrix
#2.  get the value of the matrix
#3.  set the value of the inverse
#4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set (or Cache) the inversed matrix.
  setinv <- function(solve) i <<- solve
  
  ## Get the cached inverse matrix
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}

## The following `cacheSolve` function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##    then`cacheSolve` will retrieve the inverse from the cache.

## At the moment giving: Error in x$getinv : $ operator is invalid for atomic vectors
cacheSolve <- function(x, ...) { 
  i<- x$getinv()
  
  ##Check if the inverse matrix already exist in the cache. If so, do not do the inverse again..
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  ##Since the inverse matrix doesn't exist, get the matrix data
  data <- x$get()
  
  ##Do the inverse
  i<- solve(data, ...)
  
  ##Set the cache again
  x$setinv(i)
  i
  
}

