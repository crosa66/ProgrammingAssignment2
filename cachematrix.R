## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "array" object that can cache its inverse. 
##To do this, an empty variable is created, the matrix is ​​defined and stored,
##and the inverse is subsequently calculated.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL

  set <- function(y) {
    x <<- y
    i <<- NULL

  }
  get <- function() x 
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i 

  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##This function calculates the inverse of the special "matrix" 
##returned by makeCacheMatrix previously, to do this the inverse of the matrix
##stored in makeCacheMatrix is ​​rescued, it is evaluated if the cache is null, 
##and if it is not, then the inverse is calculated with solve ()
##and the result is displayed.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  
  #Condición de error
    if (!is.null(i)) {
    message("getting cached data")
    return(i)
    }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i 
}
