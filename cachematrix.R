## makeCacheMatrix is a function that sets and gets a matrix and its inverses

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function (y) { ##to set matrix value
        x <<- y
        inv <<- NULL
    }
    get <- function() {x} ##to get matrix value
    setinverse <- function(inverse) {inv <<- inverse} ##to set inverse value
    getinverse <- function() {inv} ##to get inverse value
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    ## list created to return values

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) { ##checking to see if inverse is cached
        message("Displaying Cache Data")
        return(inv) ##returns cached value if present
    }
    mat <- x$get()
    inv <- solve(mat,...) ##does inverse calculation
    x$setinverse(inv)
    inv ##returns inverted value
        
}
