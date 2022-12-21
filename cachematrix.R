## These two functions work in tandem to cache the inverse of a matrix
## Instead of calculating the inverse a second time, if cached inverse
## exists, the cache is read from memory

## First, define the list of functions that will be used to cache the
## inverse of the matrix, namely:

## set - write the starting matrix to memory
## get - read the starting matrix
## setinv - calculate and cache the matrix inverse
## getinv - read the cached inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- matrix(NaN)
    set <- function(y) {
        x <<- y
        i <<- matrix(NaN)
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Second, call the functions from makeCacheMatrix to either calculate the
## inverse of the matrix or read the cache, as needed

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!all(is.na(i))) { ## check if cached matrix is null; if not, read cache
        message("retrieving cached inverse")
        return(i)
    }
    data <- x$get() ## read starting matrix 'x'
    i <- solve(data,...)
    x$setinv(i) ## calculate the inverse of 'x'
    i ## Return a matrix that is the inverse of 'x'
}
