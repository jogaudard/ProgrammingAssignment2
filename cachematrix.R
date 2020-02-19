## The idea here is to calculate the inverse matrix only the first time, and then to get the inversce matrix from the cache. It is apparently much faster in case of big matrix.

## the makeCacheMatrix creates a list of fonctions setting and getting the content of x and the inverse of x


makeCacheMatrix <- function(x = matrix()) {
        invx <- NULL
        set <- function(y) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invx <<- inv
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




## The first time cacheSolve is run, it calculates the inverse of the matrix (using Solve) and store it in the cache (setinv()). Then it retrieves the value of invx from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invx <- x$getinv()
        if(!is.null(invx)) {
                message("getting cached data")
                return(invx)
        }
        data <- x$get()
        invx <- solve(data, ...)
        x$setinv(invx)
        invx
}

