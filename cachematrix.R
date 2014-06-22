## Caching the Inverse of a Matrix - R program (as part of the coursera data science course)

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        q  <- NULL
        set  <- function(y){
                x <<- y
                q <<- NULL 
        }
        get  <- function() x
        setinverse  <- function(inverse) q  <<- inverse
        getinverse  <- function() q
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        q  <- x$getinverse()
        
        if (!is.null(q)){
                message("getting cached data")
                return(q)
        }
        
        data  <- x$get()
        
        q  <- solve(data, ...)
        x$setinverse(q)
        q
        ## Return a matrix that is the inverse of 'x'
}
