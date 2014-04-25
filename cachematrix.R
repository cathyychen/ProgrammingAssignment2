
###  This makeCacheMatrix function create an object that stores a numeric matrix 
###  and cache's its inverse of matrix x. Assume that the matrix x supplied is 
###  always invertible.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



###  cacheSolve function computes the inverse of a matrix using the object returned by makeCacheMatrix above.
###  If the matrix has not changed and the inverse has already been calculated, then this function should
###  retrieve the inverse from the cache. If the matrix has not changed but the inverse has not been calculated,
###  or if the matrix has changed, this function will calcuate the inverse of the matrix and cache its inverse.
###  Assume that the matrix x supplied is always invertible.
###
###  x.cache   an object stores the matrix x and cache's its inverse
###  x         the matrix that we want to get its inverse


cacheSolve <- function(x.cache,x, ...) {
        i <- x.cache$getinverse()
        # if the matrix x hasn't changed and the inverse has already been calculated,
        # retrieve the inverse from the cache
        if (x.cache$get()==x &&!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        
        # If the matrix x has not changed but the inverse has not been calculated,
        # or if the matrix x has changed, calcuate the inverse of the matrix x and cache its inverse.
        x.cache$set(x)
        data <- x.cache$get()
        i <- solve(data, ...)
        x.cache$setinverse(i)
        i
}



###################################################
#
#                  Example
#
###################################################


#   x=matrix(c(1,2,3,5),nrow=2)      ### create matrix x
#   x.cache=makeCacheMatrix(x)       ### create the cache object of matrix x
#   cacheSolve(x.cache,x)            ### calcluate the inverse of matrix x and cache it, since no inverse has been cached yet
#   cacheSolve(x.cache,x)            ### retrieve the inverse of matrix x, since x has not changed and inverse has already been cached

#   x=matrix(c(2,1,9,8),nrow=2)      ### change matrix x
#   cacheSolve(x.cache,x)            ### calcluate the inverse of matrix x and cache it, since matrix x has changed.






