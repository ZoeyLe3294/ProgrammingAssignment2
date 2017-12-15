## Put comments here that give an overall description of what your
## functions do
##Create a 2 functions that cache a inverse of a matrix
## Write a short comment describing this function
##Create a speacial matrix whose inverse can be cached
makeCacheMatrix <- function(x = matrix()) {
        Inv = NULL
        set = function(y) {
                x <<- y
                Inv <<- NULL
        }
        get = function() x
        setInv = function(inverse) Inv <<- inverse
        getInv = function() Inv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## Write a short comment describing this function
## Create function compute the inverse of the matrix.
## It will get the result if the reverse is computed, otherwise it will compute the inverse 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv = x$getInv()
        if (!is.null(Inv)) {
          message("getting cached data")
          return(Inv)
        }
        data = x$get()
        Inv = solve(data, ...)
        x$setInv(Inv)
        Inv
}
