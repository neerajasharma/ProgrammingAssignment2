##  Caching the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # set the value of the matrix
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inverse) i <<- inverse
  
  #get the value of the inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
