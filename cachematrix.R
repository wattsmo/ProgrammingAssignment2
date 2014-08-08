
## The two functions below work together to provide a method of
## caching the inverse of a square matrix so the computational
## expensive solve() functions is only called once for each
## unique matrix.

## the makeCacheMatrix function takes one variable representing a
## matrix which should be square.  The function returns a list of
## four functions which:
##     get the matrix; set the matrix;
##     get the matrix inverse; set the matrix inverse
## each time the matrix is set the value of the inverse is cleared

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) inv <<- inverse
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## the function cacheSolve takes an argument representing the
## matrix construct (returned by makeCacheMatrix function) and
## returns the inverse of the matrix. If the matrix construct
## already holds the inverse this funciton returns that otherwise
## it calls solve to calculate the inverse and stores it in the
## matrix contstruct be for returning the calculated value.
## additional arguments are passed on to solve if called.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getsolve()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setsolve(inverse)
    inverse
}

