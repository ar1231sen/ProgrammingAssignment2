## Programming assignment 2 functions

## Caches the vector

makeCacheMatrix <- function(x = matrix()) {
    
    mCache <- NULL
    
    set <- function(y){
        x <<- y
        mCache <<- NULL
    }
    
    get <- function() x
    
    setsolve <- function(solve) mCache <<- solve
    getsolve <- function() mCache
    
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    
}


## Checks cache and returns data from cache if found else calculates solve

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    mCache <- x$getsolve()
    
    if(!is.null(mCache))
    {
        message("getting cached data")
        return(mCache)
    }
    
    data <-x$get()
    mCache <- solve(data,...)
    x$setsolve(mCache)
    mCache
}
