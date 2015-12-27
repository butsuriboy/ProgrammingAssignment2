## The following functions are used to create a matrix type object
## (which we will call a cache matrix), which can also store a copy of its
## inverse, once calculated, to avoid duplicate computations every time the
## inverse is required.  The other function calculates the inverse of the
## cache matrix object, if needed, or pulls from the cached inverse, if it
## has already been determined.


## makeCacheMatrix creates a list to store a matrix (specified upon
## initialization), and methods to set and get the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## The flag to specify if the inverse has been calculated yet
    ## or not
    inv <- NULL

    ## A method to store a new matrix in this object
    set <- function(y){
        x <<- y
        inv <<- NULL
    }

    ## A method to return the matrix that is stored
    get <- function() x

    ## A method to set the matrix inverse
    setinv <- function(inverse) inv <-- inverse

    ## A method to return the inverse
    getinv <- function() inv

    ## The list containing all the methods/ variables stored that is returned
    ## upon the calling of this function.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve takes a makeCachMatrix object and returns the inverse of
## the stored matrix. If the inverse has already been calculated, it
## returns the cached matrix instead of re-calculating, but if it hasn't
## been calculated before, it utilizes the internal method "solve" to
## calculate the inverse.
cacheSolve <- function(x, ...) {
    ## Store the inverse stored in the makeCacheMatrix object
    inv <- x$getinv()

    ## Check if this inverse has been calculated/ stored before
    if(!is.null(inv)){
        ## Since the inverse has already been calculated before,
        ## can just return the inverse from the cache.
        message("Getting cached inverse")
        return(inv)
    }

    ## Since didn't have the inverse calculated before, need to
    ## calculate, store in cache, and return.

    ## First, get the original matrix
    orig_matrix <- x$get()

    ## Now, calculate the inverse
    inv <- solve(orig_matrix)

    ## Then, set the inverse in the original object, so can keep the
    ## cache for later
    x$setinv(inv)
    
    ## Finally, return the inverse calculated
    inv
}
