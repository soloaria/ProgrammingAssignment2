## The following functions works in conjunction to create a special matrix
## that is able to cache its inverse.
## The first function, makeCacheMatrix, takes an invertible matrix, and returns
## a special CacheMatrix object that is really a list of four components:
## 1. set - a function that stores the input matrix in a global variable 'x'.
## 2. get - a function that retrieves the input matrix from the global variable 'x';
## 3. setinverse - a function that caches the inverse of the input matrix and
## stores it in a global variable 'inv'.
## 4. getinverse - a function that retrieves the inverse from the global variable 'inv'.
## The second function, cacheSolve, takes the CacheMatrix object and solves
## for the inverse of the invertible matrix represented by CacheMatrix object;
## the function returns the cached inverse if a cache exists, if the cache doesn't exist,
## the function returns a solved inverse 
## and stores it in the cache of the CacheMatrix object.


## makeCacheMatrix takes in an invertible matrix and creates a special
## CacheMatrix object.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
          x <<- y
          inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function takes in the CacheMatrix object and returns the inverse
## of the invertible matrix represented by the CacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
