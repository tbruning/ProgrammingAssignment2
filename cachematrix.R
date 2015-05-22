## This code is for Programming assingment2 for R programming course
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Set and get the value of matrix
## set and get the value of inverse of the matrix

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

## Example run
x <-  rbind(c(5,10),c(10,5))
m <- makeCacheMatrix(x)
m$get()
##      [,1] [,2]
## [1,]   10   20
## [2,]   20   10
## First time without cache
cacheSolve(m)
##             [,1]        [,2]
## [1,] -0.03333333  0.06666667
## [2,]  0.06666667 -0.03333333
## second time execution with cache
## > cacheSolve(m)
## This data is from cache.
##             [,1]        [,2]
## [1,] -0.03333333  0.06666667
## [2,]  0.06666667 -0.03333333
##
## 