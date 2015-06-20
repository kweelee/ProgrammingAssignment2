## This function creates a special "matrix" object that can cache its inverse.
## Function makeCacheMatrix got 4 function, set, get, setInverse, and getInverse
## Function set will "store" the input matrix
## Function get will "retrieve" the inputed matrix
## Function setInverse will solve the input matrix and save it
## Function getInverse will get the inverse matrix if there is exist

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## cacheSolve will return the inverse of 'x'(x is a special matrix object)
## First it will checked if the Inverse of this matrix exist
## If yes, it will return the Inverse value.
## if not, it will compute the Inverse value using the actual matrix data from get function
## and finally save the value of inverse data using setInverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
