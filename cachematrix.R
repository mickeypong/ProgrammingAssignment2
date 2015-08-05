## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function calculates the inverse matrix of the special "matrix" created with the 
## makeCacheMatrix function. However, it first checks to see if the inverse matrix has already been 
## calculated. If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, 
## it calcualtes the inverse matrix of the data and sets the value of the inverse matrix in the cache
## via the setInverse function.

cacheSolve <- function(cacheX, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- cacheX$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    } 
    x <- cacheX$get()
    inv <- solve(x, ...)
    cacheX$setInverse(inv)
    inv
}

