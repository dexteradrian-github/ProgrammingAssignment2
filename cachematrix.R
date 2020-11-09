## My Submission for Programming Assignment 2
## Coursera R-Programming JHU Week 3

## creates a special "matrix" object that can cache its inverse
## Used makeVecotor as a basis and made the necessary changes

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function(y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() {x}
        set_inv <- function(inverse) {mat_inv <<- inverse}
        get_inv <- function() {mat_inv}
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

## computes the inverse of the special "matrix" returned by function above
## if inverse has already been calculated, retrieve the inverse from the cache
## Used cachemean as an example

cacheSolve <- function(x, ...) {
        mat_inv <- x$get_inv()
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
        }
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$set_inv(mat_inv)
        mat_inv
}