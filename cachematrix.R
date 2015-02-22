## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL # Storing the result of the inverse 
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) minv <<- inverse
        getinverse <- function() minv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
minv <- x$getinverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data)
        x$setinverse(minv)
        minv
}