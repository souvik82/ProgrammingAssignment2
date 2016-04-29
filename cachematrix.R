## makeCacheMatrix(): creates a special “matrix” object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
  # use `<<-` to assign a value to an object in an environment 
  # different from the current environment. 
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set =set,get=get,setinverse=setinverse,getinverse=getinverse)
}

## cacheSolve(): computes the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, it’ll return the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    ## get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Test the result
## r <- rnorm(48)
## x <- matrix(r, nrow=4, ncol=4)
## y <- makeCacheMatrix(x)
## cacheSolve(y)
