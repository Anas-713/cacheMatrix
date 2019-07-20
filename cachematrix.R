## Code to Cache the inverse of a matrix .

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {          ## argument given to the function with default mode of "matrix"
inv<-NULL                                            ## initializing inv as NULL
set <- function(y) {                                 ## defined set function
  x <<- y
  inv <<- NULL                                       ## in case of new matrix,reset inv to NULL
}
get<-function()x                                     ## define the get function
setinverse <- function(inverse) inv <<- inverse      ## assign value of inv
getinverse <- function() inv                         ## gets the value of inv when called
list(set = set, get = get,setinverse = setinverse,getinverse = getinverse) ## defining the list to the function
}


## This function computes the inverse of the matrix returned by the makeCacheMatrix() above.
## If the inverse has already been calculated then cacheSolve will fetch the inverse from the cache.

cacheSolve <- function(x, ...) {
 inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
