## This code is for Programming assingment2 for R programming course
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Set and get the value of matrix
## set and get the value of inverse of the matrix
## Additional comments
## This function takes a 'inersable' matrix and creates the inverse.  
makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
        x <<- y
        inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
# This function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If yes, it gets the result from cache and skips the
# computation. If not, it computes the inverse, sets the value in the cache 

## Additional comments
## This function creates the inversed matrix.  It displays the matrix and lets you know if it is from a new compute, or was it retrieved from cache.

cacheSolve <- function(x, ...) {
    inver <- x$getinverse()
    if(is.null(inver)) {
        message("This data is new compute.")
    } else {
        message("This is from cache!")
        return(inver)
    }
    data <- x$get()
    inver <- solve(data)
    x$setinverse(inver)
    inver
}

