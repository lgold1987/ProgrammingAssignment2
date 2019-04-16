## Functions do matrix inversion and cache the inverse of the matrix

## makeCacheMatrix creates a list containing a function to (1) set the matrix value, (2) get the matrix value
##(3)set the inverse matrix, and (4) get the value of of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The following function returns the inverse matrix and check to if the value has been computed.
## If the value has been computed it gets the result and skips the computation
## Otherwise, it computes the inverse and sets the value in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <-x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
