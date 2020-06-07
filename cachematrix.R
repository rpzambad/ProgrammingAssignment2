## Two functions are written to use cache to avoid recomutation of inverse of
## matrix.

## Creating a matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
        s <- NULL
        
        ## Set the value of Matrix
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        
## Get the value of Matrix
        get <- function() x
        
## Setting Inverse of the Matrix
        setInv <- function(solve) s <<-solve
        
## Getting the inverse of the Matrix
        getInv <- function() s
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)

}


## A function to compute the inverse of the Matrix if it hasn't
## been calculated.

cacheSolve <- function(x, ...) {
        s <- x$getInv()
        
## Return the inverse of Matrix if its computed
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
## Compute the inverse of matrix and save it for future use
        s <- solve(data, ...)
        x$setInv(s)
        s
}

