## makeCacheMatrix: 
##     this function creates a special "matrix" object 
##     that can cache its inverse

## cacheSolve:
##     this function computes the inverse of the special
##     "matrix" returned by makeCacheMatrix.  If the inverse
##     has already been calculated (and the matrix has not 
##     changed), then cacheSolve will retrieve the inverse
##     from the cache

## Directions for use:
##     1.  x <- matrix(rnorm(1000000,2,4),1000,1000)  ## create matrix
##     2.  x2 <- makeCacheMatrix(x)                   ## cache "matrix"
##     3.  x3 <- cacheSolve(x2)                       ## calculate inverse
##     4.  x3 <- cacheSolve(x2)                       ## inverse from cache

## makeCacheMatrix:
##     Create a special "matrix", which is a list containing a function to:
##         1.  Set the value of the "matrix"
##         2.  Get the value of the "matrix"
##         3.  Set the value of the inverse "matrix"
##         4.  Get the value of the inverse "matrix"
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve:
##     Calculate the inverse of the special "matrix" created in
##     makeCacheMatrix.  It checks to see if the inverse has already
##     been calculated.  It is has, the inverse is pulled from the 
##     cache.  Otherwise, it is calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
