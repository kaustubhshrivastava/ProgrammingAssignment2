## Assignment 2 of the Coursera course for R

## Returns an matrix object for storing the matrix and its inverse

makeCacheMatrix <- function(matStore = matrix())
{
  #Matrix storage location
  inverseStore <- NULL
  
  set <- function(y)
  {
    matStore <<-y
    inverseStore <<-NULL
  }
  get <- function() matStore
  
  setInverse <- function(inverse)
  {
    inverseStore <<- inverse
  }
  getInverse <- function() inverseStore
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function returns the inverse of the matrix. 
## If the matrix inverse is in the cache, it returns
## the cashed results rather than recaclulating.

cacheSolve <- function(inverseMatrixObject) {
  inverseMatrix <- inverseMatrixObject$getInverse()
  if(!is.null(inverseMatrix)) {
    message("Cached inverse matrix found!")
    return(inverseMatrix)
  }
  data <- inverseMatrixObject$get()
  invertedMatrix <- solve(data)
  inverseMatrixObject$setInverse(invertedMatrix)
  invertedMatrix
}
