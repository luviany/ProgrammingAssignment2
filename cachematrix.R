## Project Assigment R Programming
## Assignment: Caching the Inverse of a Matrix

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # initialize the stored inverse value to NULL
  inverseValue <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    inverseValue <<- NULL # matrix has changed, reassign to NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(inverse) inverseValue <<- inverse
  
  # get inverse of matrix
  getinverse <- function() inverseValue
  
  # list of functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # get inverse
  inverseValue <- x$getinverse()
  
  # if inverseValue exists, check if already cached
  # if yes, return cached inverse
  if(!is.null(inverseValue)) {
    message("getting cached data")
    return(inverseValue)
  }
  
  # if not, get matrix
  data <- x$get()
  
  # compute inverse of matrix
  inverseValue <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inverseValue)
  
  # return inverse
  print(inverseValue)
}