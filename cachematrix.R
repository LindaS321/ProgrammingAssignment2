## A pair of functions that cache the inverse of a matrix
## 

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  ## Set the value of the matrix to value passed as y
  setMatrix = function(y) {
    ## use `<<-` to assign a value to an object in an environment 
    ## different from the current environment. (create a cache)
    ## Set the value of the matrix to value passed as y
    x <<- y
    ## set the variable that holds the inverse to NULL
    inv <<- NULL
    
  }
  getMatrix <- function() x
  ## save the inverse of the matrix to inv
  setInverse <- function(solve) inv <<- solve
  ## retrieve the inverse of the matrix
  getInverse <- function() inv
  ## create a list which holds get and set inverse functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)

}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated and the matrix has not changed
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  ## Determine if the inverse has already been calculated and the matrix has not changed
  i <- x$getInverse()
  ## If the inverse has already been calculated returned cached value 
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ## if the inverse has not been calculated, then do it with solve
  ## get the data
  data <- x$getMatrix()
  ## Use the solve function to return the inverse of the matrix
  i <- solve(data, ...)
  ## Use the set function to set new value of matrix
  x$setInverse(i)
  ## return the matrix's inverse
  i
    
}
