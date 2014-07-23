
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    # Inverse of a square matrix with the solve function
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
    setmatrix = setmatrix,
    getmatrix = getmatrix)
}

## This function calculates the inverse of the matrix created with
## makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatrix()
        if(!is.null(m)) {
            # if the inverse has been already calculated
            # it retrieves the inverse from the cache
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
