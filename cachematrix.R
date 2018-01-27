# This function creates a special "matrix" object that 
# can cache its inverse.
#
# @param	a matrix X. Optional. The matrix can also 
#			be imported using the set() method
#
# 	set() 			sets the matrix
# 	get() 			gets the matrix
# 	setInverse() 	sets the inverse of the matrix
# 	getInverse() 	gets the inverse of the matrix
#
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, 
         get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

# This function computes the inverse of the special 
# "matrix" returned by makeCacheMatrix above. If the
# inverse has already been calculated (and the matrix
# has not changed), then the cachesolve should retrieve
# the inverse from the cache.
#
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    m0 <- x$get()
    inv <- solve(m0)  # det(m0) != 0
    x$setInverse(inv)
    inv
}

#
# PLAYGROUND
#
# Create using constructor:
# 
# y <- matrix(rnorm(9),nrow=3,ncol=3)
# z <- makeCacheMatrix(y)
# cacheSolve(z)
#
# Create using set(): 
#
# x <- makeCacheMatrix()
# x$set(matrix(1:4,nrow=2,ncol=2))
# cacheSolve(x)
#
# TEST: confirm identity matrix
#
# x$get() %*% cacheSolve(x)
#
# round(z$get() %*% cacheSolve(z),digits=13)
#

