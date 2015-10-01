## Programming Assignment 2
## The purpose of this assignemnt is to create a "Matrix" object that carries not only its data and
## functions required to get/set, it also carries the Inverse of the matrix.

## makeCacheMatrix - Sets up a Matrix object, which contains:
## x - The matrix itself
## inv - The inverse of matrix x - NULL if not yet cached
## set() - A function to load the matrix
## get() - A function to retrieve the contents of the matrix
## setinv() - A function to Set the contents of the inverted matrix
## getinv() - A function to Get the contents of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(
        set = set, 
        get = get,
        setinv = setinv,
        getinv = getinv)
}


## cacheSolve - A function that takes a Matrix object as an argument and - if not already cached - calculates
## and caches its inverse. The matrix inverse is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    if (!is.null(x$getinv())) {
        print("Returning cached inverse")
        return(x$getinv())
    }
    
    x$setinv(
        matrix(
            data = as.vector(x$get()),
            nrow = ncol(x$get()),
            ncol = nrow(x$get()),
            byrow = TRUE
        )
    )
    return(x$getinv())
}
