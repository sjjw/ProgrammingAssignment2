## The functions below are used to cache the matrix inverse, to avoid
## recalculating it again and again. An inverse of a large matrix can
## be time consuming and caching will save this processing time

## The below function "makeCacheMatrix" is used to make a special object
## that can cache the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The below function checks if the inverse of the matrix exists
## If it exists then it return the already calculated value
## Else it calculates the inverse and caches it using the above function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) # This function is used to calculate inverse in R
        x$setinverse(m)
        m
}
