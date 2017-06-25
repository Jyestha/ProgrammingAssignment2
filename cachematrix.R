## Objective is to write a function that 
##    Set the value of a matrix
##    Get the value of a matrix
##    Set the inverse of the given matrix
##    Get the inverse of the given matrix

## Write a second function that will solve and cache the inverse of the matrix function

##This is the first function. It has 4 parts
## curMatrix is the object on which the function is being set and the object has been assigned the data type of matrix
## The setMatrix function allows the value of the matrix to be dynamic and the use of <<- operator will make it available
## outside the function
makeCacheMatrix <- function(curMatrix = matrix()) {
  invCache <- NULL
  setMatrix <- function(newMatrix = matrix()) {
    curMatrix <<- newMatrix
    invCache <<- NULL
  }
## getMatrix function retrieves the curMatrix  
  getMatrix <- function() {
    curMatrix
  }
## setInverse caches a value as the inverse matrix
  setInverse <- function(newInverse) {
    invCache <<- newInverse
  }
## getInverse retrieves the cache
  getInverse <- function() {
    invCache
  }
## The list command allows subfunctions to be accessed from the parent command
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


## The following is the second function
## It calculates the inverse of the matrix created with the makeCacheMatrix function
## madeMatrix is the entire function of makeCacheMatrix
## The value of invCache is assigned to invMatrix
## If the value of invMatrix is not null invMatrix is output based on cached data

cacheSolve <- function(madeMatrix, ...) {
  invMatrix <- madeMatrix$getInverse()
  if(!is.null(invMatrix)) {
    message("Getting cached data.")
    return(invMatrix)
  }
## The inverse of the matrix is calculated here and returned
  data <- madeMatrix$getMatrix()
  invMatrix <- solve(data)
  madeMatrix$setInverse(invMatrix)
  invMatrix
}

## Testing if the code works
m = makeCacheMatrix()
m$setMatrix(rbind(c(5,6), c(6,5)))
m$getMatrix()
     [,1] [,2]
[1,]    5    6
[2,]    6    5
cacheSolve(m)
           [,1]       [,2]
[1,] -0.4545455  0.5454545
[2,]  0.5454545 -0.4545455

