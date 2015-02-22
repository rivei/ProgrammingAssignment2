## Below two functions are used to create a special object that stores 
## a matrix and caches the inverse of that matrix

## The function 'makeCacheMatrix' creates a special 'matrix' object that
## can cache its inverse. It contains a list of functions to
## 1.  set the matrix
## 2.  get the matrix
## 3.  set the inverse of the matrix
## 4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve = matrix()) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The function 'cacheSolve' computes the inverse of the special 'matrix
## returned by 'makeCacheMatrix' above. If the inverse has already been
## calculated (and the matrix has not changed), then `cacheSolve` retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached matrix inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
