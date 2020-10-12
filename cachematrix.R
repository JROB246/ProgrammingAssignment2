## These functions are used to store a matrix 
## that can be recalled from the memory 
## and inversed, using the 'cacheSolve' function.

## This function stores a matrix to the cache.
## It specifies that if no matrix is stored in the 
## cache, the vale of the cache is 'NULL'. This is 
## utilised in the cacheSolve function below. This 
## function also specifies a number of values related 
## to the cached matrix that can be recalled in the 
## command line (e.g. x$getinverse) and are utilised 
## in the cacheSolve function below.

makeCacheMatrix <- function(x = matrix()) {
        inmx <- NULL
        set <- function(y){
                x <<- y
                inmx <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inmx <<- inverse}
        getInverse <- function() {inmx}
        list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


## This function provides the inverse of the cached matrix. 
## If there is no cached matrix, the function is in-operable.
## If there is an inversable cached matrix (i.e. (!is.null(inmx)) = TRUE),
## 'mx' is retrieved from the cache and passed to the 'solve()' function.
## The function ultimately returns 'inmx'; the inverse of the cached matrix. 

cacheSolve <- function(x, ...) {
        inmx <- x$getInverse()
        if(!is.null(inmx)){
                message("getting cached data")
                return(inmx)
        }
        mx <- x$get()
        inmx <- solve (mx, ...)
        x$setInverse(inmx)
        
        inmx
}
