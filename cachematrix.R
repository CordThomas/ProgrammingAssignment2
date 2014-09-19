## makeCacheMatrix is a method to expose a matrix that caches its inverted state.  
## The method takes an argument that should be a matrix.
## cacheSolve takes a matrix and inverts it through the makeCacheMatrix method 
## after checking that the inverted matrix is not already in the cache.

## A method for a cachable inverse matrix
## invert is the matrix to be inverted.

makeCacheMatrix <- function(invert = matrix()) {
    i <- NULL
    set <- function(matrix) {
        invert <<- matrix
        i <<- NULL
    }
    get <- function() invert
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Method to return an inverted matrix from the one supplied.
## Method checks that the supplied arguement is a matrix but performs
## no other checks nor does it have any other boundary condition checks.
## x is an instance of the makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## must be a better way to check that this is a matrix.
    ## could also check that this is square but assignment says to assume
    ## that x supplied is square
    if (class(x) != "list") {
        message ("First arguement must be a matrix")
        return(-1)
    }
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached inverted matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
