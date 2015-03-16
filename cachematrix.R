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


## this function checks if the matrix whose inverse we want to calculate
## has already cached the inverse by querying the getInverseMatrix method;
## if it has, it returns the cached answer, other wise it calculates the
## inversematrix and stores the answer for future queries

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
