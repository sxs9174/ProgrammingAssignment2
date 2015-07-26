## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates a list of getters and setters for matrix
## and inverse of matrix.

## Write a short comment describing this function
## makeCacheMatrix creates a list object of getters and setters
## for matrix and its inverse. x is the matrix, i is the inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  setmatrix <- function(y) {
    x <<- y
    i <<- NULL
  }
  getmatrix <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(setmatrix = setmatrix, 
       getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## x is the special matrix object created in makeCacheMatrix
## function. First get the inverse of the matrix by getinverse
## call. If the inverse is empty, then get the matrix from
## the getmatrix call, get its inverse by calling solve and
## return the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("get matrix inverse from cache")
    return(i)
  }
  data <- x$getmatrix()
  i <- solve(data)
  x$setinverse(i)
  i
}