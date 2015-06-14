# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.

# makeCacheMatrix: This function creates a special "matrix" object that can
# cache its inverse. It is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inverse.matrix <- NULL
    set <- function(y) {
        x <<- y
        inverse.matrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse.matrix <<- inverse
    getinverse <- function() inverse.matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix. If the inverse has already been calculated, 
# then the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse.matrix <- x$getinverse()
    if(!is.null(inverse.matrix)) {
        message("getting cached data.")
        return(inverse.matrix)
    }
    data <- x$get()
    inverse.matrix <- solve(data)
    x$setinverse(inverse.matrix)
    inverse.matrix
}

