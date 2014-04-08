## This file gives a pire of function that can cache the inverse of a Matrix


## This function creates a special matrix, which is a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the matrix of the inverse
## 4. get the matrix of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special 'matrix'. It first check to see if the inverse have 
## already been calculated. If so, it gets the inverse from the cache and skip the computation. Else, it calculates
## the inverse of the data and sets the matrix of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}


matrix= matrix(1:4, nrow = 2, ncol = 2)
newMatrix = makeCacheMatrix(matrix)
cacheSolve(newMatrix)
