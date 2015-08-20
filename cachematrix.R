## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This is a constructor that contains getters
#and setters for both the matrix parameter
#and its inverse by putting the contents in a list.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

#Assume x is both a square and invertible matrix.
#This takes the inverse of a matrix x and returns it.
#First it checks if the inverse was already computed.
#If so, it simply returns the stored inverse matrix.
#Otherwise, it gets the contents of the matrix and inverts
#it via the solve function, stores the inverse (setinverse) 
#so it can be computed easily in the future, and returns it.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
