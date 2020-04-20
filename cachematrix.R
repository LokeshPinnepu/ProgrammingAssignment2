## This function takes matrix as input and set, get the values of the matrix.
## also to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  
  getMatrix <- function() x 
  setInverse <- function(inverse) invMatrix <<- inverse 
  getInverse <- function() invMatrix                    
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)

}


## This function takes the output of the  makeCacheMatrix(matrix) as input.
## If the inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
## and set the invertible  matrix by using the solve function.
## If the inverse matrix from makeCacheMatrix((matrix) has some value in it, it returns the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting Cached Invertible Matrix")
    return(invMatrix)
  }
 
  MatrixData <- x$getMatrix()
  invMatrix <- solve(MatrixData, ...)
  x$setInverse(invMatrix)
  return(invMatrix)
}
