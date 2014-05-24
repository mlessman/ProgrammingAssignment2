## these two functions work together to store a matrix
## and then caluculate the inverse of the matrix
## if the inverse matrix has already been stored, the 
## cache will return the result instead of recalculting

## sets variable x as a matrix
## makeCacheMatrix function creates a list of other functions

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        setcache <- function(y) {
                x <<- y
                m <<- NULL
        }
        getcache <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(setcache = setcache, getcache = getcache,
             setinverse = setinverse,
             getinverse = getinverse)

}


## checks to see if inverse matrix is cached
## if not already cached, solves for inverse, then sets the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$getcache()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
