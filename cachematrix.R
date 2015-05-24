## This function matrix object that can cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  # initialize the stored inverse value to NULL
  inv <- NULL
  
  # set value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL # matrix has changed, reassign to NULL
  }
  
  # get value of matrix
  get <- function() x
  
  # set inverse of matrix
  setinverse <- function(inverse) inv <<- inverse
  
  # get inverse of matrix
  getinverse <- function() inv
  
  # return a list containing all functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the matrix returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  
  # get inverse
  inv <- x$getinverse()
  
  # if inverse exists, check if already cached
  # if yes, return cached inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if not, get matrix
  data <- x$get()
  
  # compute inverse of matrix
  inv <- solve(data, ...)
  
  # cache inverse of matrix
  x$setinverse(inv)
  
  # return inverse
  inv
}
