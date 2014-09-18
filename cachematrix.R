## The following functions creates an "object" consisiting of 2 matrixes 
## stored in a "cache" accessible only through 4 functions (kind of methods)
## to read, write the matrix and its inverse.

## The makeCacheMatrix creates two matrix in the cache environment called Mat
## to store the input matrix and invMat to store the output matrix, that in this
## case is intended to be the inverse of input matrix. It creates 4 functions
## to read and write in the cache the input matrix and the output matrix.
## If a matrix is passed it stores the matrix in the cache.

makeCacheMatrix <- function(Mat = matrix()) {
        
        invMat <- NULL
        set <- function(y) {
                Mat <<- y
                invMat <<- NULL
                }
        
        get <- function() Mat
        setinv <- function(inv) invMat <<- inv
        getinv <- function() invMat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function accept as input an object created with the previous function.
## Before using this function one object (list) from previous function must
## be created and a matrix input must be stored in the cache.
## it returns the inverse of a matrix stored in the cache if already 
## calculated for the input matrix or calculate it and store in the cache for future use.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
