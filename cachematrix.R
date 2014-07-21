## The makeCacheMatrix and cacheSolve functions are used to cache the
## potentially time consuming process of calculating the inverse of a
## matrix.  

## makeCacheMatrix is a function that can cache its inverse matrix.
## It creates a list that you can use to set the matrix, get the matrix,
## set the inverse of the matrix, and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function that returns the inverse matrix of 'x'. 
## If the inverse was previously computed, it returns the cached 
## matrix inverse with the message "getting cached data".

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
