## These functions implement caching of the matrix inverse function, solve.
##
## To make use of them, use the makeCacheMatrix function to create a wrapper
## around a matrix and then call cacheSolve on this matrix where you would
## otherwise have called solve on the original matrix.
##
## Example:
## > m  <- matrix(c(1, 2, 3, 4), 2, 2)
## > cm <- makeCacheMatrix(m)
## > cacheSolve(cm)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cm)
## getting cached inverse
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5


## Creates a wrapper around the matrix passed as the first and only argument.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Returns the inverse of a matrix wrapper created with makeCacheMatrix and
## passed as the first and only argument. Prints a message when returning 
## cached value.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
