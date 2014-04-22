
## The first function, makeCacheMatrix creates a list containing functions to
## set the values of the matrix - set
## get the values of the matrix - get
## set the inverse of the matix - setinverse
## get the inverse of the matrix - getinverse



makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
   set <- function(y) {
     x <<- y
     m <<- NULL
   }
   get <- function() x
   setinverse <- function(solve) m <<- solve
   getinverse <- function() m
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
 }
    
 

## cacheSolve calculates the inverse of the matrix using makeCacheMatrix. 
## It first checks to see if the inverse has already been calculated using !is.null. 
## If the inverse has been calculated, it gets the inverse from the cache and does not compute the inverse.
## If the inverse has not been calculated it does the following -
##   it calculates the inverse of the matrix using solve 
##   it places the inverse in the cache using the setinverse function.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
     message("getting cached data")
     return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
 }
