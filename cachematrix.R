
### This makeCacheMatrix function create an object that stores a numeric matrix and cache's its inverse

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



### This cacheSolve function computes the inverse of a matrix using the object returned by makeCacheMatrix above.
### If the matrix has not changed and the inverse has already been calculated, then this function should
### retrieve the inverse from the cache. If the matrix has not changed but the inverse has not been calculated,
### or if the matrix has changed, this function will calcuate the inverse of the matrix and cache its inverse.
###
### x.cache   an object stores the matrix x and cache's its inverse
### x         the matrix that we want to get its inverse


cacheSolve <- function(x.cache,x, ...) {
        i <- x.cache$getinverse()
        if (!is.null(i)&& x.cache$get()==x) {
                message("getting cached inverse")
                return(i)
        }
        x.cache$set(x)
        data <- x.cache$get()
        i <- solve(data, ...)
        x.cache$setinverse(i)
        i
}
