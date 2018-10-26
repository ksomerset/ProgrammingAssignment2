## MakeCacheMatrix creates functions needed to cache inverse of the invertible matrix input
## Output to be used in CacheSolve function to calculate and cache inverse of matrix
## to save time and effort in future calculations that require that value as an input
makeCacheMatrix <- function(x = matrix()) {
      invMatrix <- NULL   
      setMatrix <- function(y) {  
            x <<- y
            invMatrix <<- NULL 
      }     
      getMatrix <- function() x  
      setInverse <- function(inverse) invMatrix <<- inverse 
      getInverse <- function() invMatrix 
      list (setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}
## CacheSolve uses inputs from MakeCacheMatrix to calculate value of inverse of matrix 
## and cache it to reuse value in future calculations 
cacheSolve <- function(x, ...) {
      invMatrix <- x$getInverse() 
      if(!is.null(invMatrix)) { 
            message("getting cached data")
            return(invMatrix) 
      }
      MatrixData <- x$getMatrix() 
      invMatrix <- solve(MatrixData, ...)
      x$setInverse(invMatrix)
      return(invMatrix)
}

