## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly. 
## Following pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve 
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}

## testInverse will invoke the cached matrix inversion functionality provided by
## makeCacheMatrix and cacheSolve to ensure the embedded matrix is correctly inverted.
## It will then re-invoke cacheSolve on the existing cacheMatrix to ensure the cached
## version of the inverse is utilized (per the "Retrieving the cached invers." message
## from cacheSolve())
## Input: none
## Output: The identity matrix if the matrix was correctly inverted

testInverse <- function() {
        mat <- matrix(1:16,nrow=4,ncol=4)
        mat[1,]=c(2,3,1,5)
        mat[2,]=c(1,0,3,1)
        mat[3,]=c(0,2,-3,2)
        mat[4,]=c(0,2,3,1) 
        print("Matrix to be inverted:")
        print(mat)
        if(det(mat) == 0){
                "Matrix is not invertible"
        } 
        else {
                cacheMat <- makeCacheMatrix(mat) # create a cached matrix object
                inv <- cacheSolve(cacheMat) # compute the inverse
                id <- inv %*% mat
                print("Multiplication of matrix by its inverse:")
                print(id)
                # Copare with identity matrix
                if(identical(id,diag(1,nrow(id),ncol(id))))
                        print("Correct!")
                else
                        print("Wrong!")
    }
}
