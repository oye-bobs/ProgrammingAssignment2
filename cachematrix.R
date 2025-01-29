## These functions create and cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL  ## Initialize the cached inverse to NULL

  ## Sets the matrix and resets the cached inverse.
  set <- function(y) {
    x <<- y
    i <<- NULL  ## Reset cached inverse when matrix changes
  }

  ## Gets the matrix.
  get <- function() x

  ## Sets the cached inverse.
  setInverse <- function(inverse) i <<- inverse

  ## Gets the cached inverse.
  getInverse <- function() i

  ## Returns a list of functions to set/get the matrix and its inverse.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse is already cached, it retrieves it from the cache.
cacheSolve <- function(x, ...) {
  i <- x$getInverse()  ## Attempt to retrieve the cached inverse

  if(!is.null(i)) {  ## Check if the inverse is already cached
    message("getting cached data")
    return(i)  ## Return the cached inverse
  }

  data <- x$get()  ## Get the matrix from the special object
  i <- solve(data)  ## Calculate the inverse
  x$setInverse(i)  ## Cache the calculated inverse
  i ## Return the calculated inverse
}
