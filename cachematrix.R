## Implementation of caching matrix inversion

## makeCacheMatrix()
## 
## When called, function is passed the original matrix (default is an empty matrix)
## The value is stored within the closure as the function argument
##
## Remaining getter / setter functions allow changing the original
## matrix and the inverse matrix calculation
##
## These functions are stored within a list that is returned from the function.

makeCacheMatrix <- function(x = matrix()) {
    mat.inv <- NULL
    
    set <- function(y) {
        x <<- y
        mat.inv <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(inv) mat.inv <<- inv
    
    getinverse <- function() mat.inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve()
##
## function takes an instance of the list resulting from
## a call to makeCahceMatrix().  This list is stored by the
## argument itself within the function's closure.
##
## The cached inverse of the matrix is immediately assigned to the
## return value.  If it is not null, the cached value is returned.
## Otherwise, the original matrix is retrieved from the cached list
## and it's inversion is calculated.  The result is stored in the cached list
## and then returned.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    mat.inv <- x$getinverse()
    
    if(!is.null(mat.inv)) {
        message("getting cached data")
        return(mat.inv)
    }
    
    data <- x$get()
    mat.inv <- solve(data, ...)
    x$setinverse(mat.inv)
    mat.inv
}
