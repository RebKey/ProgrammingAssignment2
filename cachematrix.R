## Coursera - R Programming Course (rprog-011)
## Assignment #2, due 2/22/2015
## Rebecca Key

## This assignment is to create a series of functions that will assist with creating and
## caching the inverse of a matrix for future use.
## 

## The first function is to create a matrix object that will hold the cached inverted matrix
makeCacheMatrix <- function(x.mat = matrix()) {
     m.mat <- NULL
     setmat <- function(y.mat = matrix()) {
          x.mat <<- y.mat
          m.mat <<- NULL
     }
     getmat <- function() x.mat
     setsolve <- function(solve) m.mat <-- solve
     getsolve <- function() m.mat
     list(setmat = setmat, getmat = getmat, setsolve = setsolve, getsolve = getsolve)
}

## The second function does the actual calculation for the inverted matrix. This takes the 
## inputted matrix and uses the function solve() to calculate the inverse of the matrix.
## Usually run this new function (cacheSolve) twice to make sure it cached the data.
cacheSolve <- function(x.mat, ...) {
     m.mat <- x.mat$getsolve()
     if(!is.null(m.mat)) {
          message("getting cache matrix data")
          return(m.mat)
     }
     data <- x.mat$getmat()
     m.inv <- solve(data, ...)
     x.mat$setsolve(m.inv)
     m.inv
}
