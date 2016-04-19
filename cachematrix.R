## These functions store new matrix object, calculate the inverse,
## and store it in cache
##
## Example
##
## > x = matrix(1:4,2,2)
## > matrixObject <- makeCacheMatrix(x)
## > cacheSolve(matrixObject)
## calculating new inversed result
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(matrixObject)
## getting cached matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5



## makeCacheMatrix(x) populates an object with new matrix and 
##stores cache of its inverse
## object properties:
##      get()
##      set(x)
##      getinverse() 
##      setinverse(xx)

makeCacheMatrix <- function(x = matrix()) {

  invMatrix <- NULL
  
  set <- function(y) { #set new object and clear cache
    x <<- y
    invMatrix <<- NULL
  }
  
  get <- function() x #return the original matrix
  
  setinverse <- function(solve) invMatrix<<- solve #set inverse cache
  
  getinverse <- function() invMatrix               #get inverse cache
  
  list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## This function returns inverse of the matrix. 
## If cached result is available, it returns cache
## If cache is empty , it calculates new result and stores it in cache

cacheSolve <- function(x, ...) {
  
  #attempt to get cache
  invMatrix <- x$getinverse()
  
  #if cache exists, return it
  if(!is.null(invMatrix)) {
    message("getting cached matrix")
  }
  #if cache is null, calculate new result and cache it
  else
  {
    message("calculating new inversed result")
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setinverse(invMatrix)
  }
  
  return(invMatrix)
}


