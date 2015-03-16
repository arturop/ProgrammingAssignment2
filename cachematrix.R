## Put comments here that give an overall description of what your
## functions do

## this function creates the matrix object and the associated 'methods'
## which implement the functionality to set and get both the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverseMatrix <- function(im) (inverseMatrix <<- im)
  getInverseMatrix <- function() inverseMatrix
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inverseMatrix <- x$getInverseMatrix()
  if(!is.null(inverseMatrix)) {
    message("getting inverse matrix")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverseMatrix(inverseMatrix)
  inverseMatrix
}
