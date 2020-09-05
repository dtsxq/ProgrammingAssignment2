
# The function below initiates a makeCacheMatrix object with set variables and functions.
# It can set the x (matrix) parameters, and return it, using the set() & get() functions.
# Likewise, it can set the inv (inverse) parameters, and return it, using the
# setInv() & getInv() functions. It caches these values.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(inverse) inv <<- inverse
        getInv <- function() inv
        list(set = set, get = get,
             setInv = setInv, getInv = getInv)
}

# This function uses the initialised object to check if it already has a cached inv
# (inverse) value. If not, then it calculates this inverse value using solve(). 
# Finally, it sets the inv value for the object x using setInv().

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
        if (!is.null(inv)) {
                message('getting cached data')
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
