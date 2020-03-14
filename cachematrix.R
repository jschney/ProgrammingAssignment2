## Title: Week3_Assignment2_Cache Matrix Inverse
## Author: jschney
## Repo: https://github.com/jschney/ProgrammingAssignment2
## Date Created: 3/14/2020

## Purpose: Create a series of functions that will cache the inverse of a matrix, rather than
##          repeatedly compute the inverse.

## The first function will set the value of a matrix, acquire the value of the matrix,
## convert and set the value of the inverse of the matrix, and finally cache the inverse of
## the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                     ## Create empty matrix
  set <- function(y) {                          ##|\
    x <<- y                                     ##|-->These rows set the value of the matrix
    m <<- NULL                                  ##|/
  }
  get <- function()x                            ## Retrieve value of the matrix
  setinverse <- function(inverse)m <<- inverse  ## Calculate and set inverse value of matrix
  getinverse <- function() m                    ## Set value of inverse of matrix
  
  list(set = get,                               ## Create list of objects
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function compute the inverse of the matrix created by makeCacheMatrix
## function above. If the inverse was previously calculated, the function will
## return the inverese matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                          ## Get inverese value of matrix if it is cached
  if (!is.null(m)) {                           ## If value is NULL, then data is cached
    message("getting cached data")
    return(m)                                  ## Otherwise calculate inverse again and return
  }
  result <- x$get()
  m <- solve(result, ...)
  x$setinverse(m)
  m
}
