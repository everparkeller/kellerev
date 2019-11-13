## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix) {
  # Arguments required -> Matrix to be inputted as x. 
  # Within the function, it will need to determine the value of the matrix and
  # create the inverse of the matrix. 
  
  inverse.Matrix <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inverse.Matrix <<- NULL
  }
  
  getMatrix <- function() x
  setInverse <- function(inverse) inverse.Matrix <<- inverse
  getInverse <- function() inverse.Matrix
  list(setMatrix=setMatrix, getMatrix=getMatrix,
       setInverse=setInverse, getInverse=getInverse)
}

## Write a short comment describing this function

## The output from the prior function can be used within this function
## in order to determine the inverse of the original matrix. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse.Matrix <- x$getInverse()
  if(!is.null(inverse.Matrix)) {
    message("getting cached invertible matrix")
    return(inverse.Matrix)
  }
  
  original.Matrix <- x$getMatrix()
  inverse.Matrix <- solve(original.Matrix, ...)
  x$setInverse(inverse.Matrix)
  return(inverse.Matrix)
}


(Test <- matrix(c(1, 3, 
                  5, 16), nrow=2, ncol=2, byrow=TRUE))

Cache.Matrix <- makeCacheMatrix(Test)
Cache.Matrix$getMatrix()
Cache.Matrix$getInverse()

cacheSolve(Cache.Matrix)

(Test <- matrix(c(2, 5, 1, 
                  8, -3, 2, 
                  -9, 11, 4), nrow=3, ncol=3, byrow=TRUE))

Cache.Matrix <- makeCacheMatrix(Test)
Cache.Matrix$getMatrix()
Cache.Matrix$getInverse()

cacheSolve(Cache.Matrix)
