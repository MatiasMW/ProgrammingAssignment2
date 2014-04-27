##  This function is able to cache potentially time-consuming computations, in
## this particular case Matix inversion


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
                x <<- y
        }
        get <- function () x
        setsolve <- function (solve) m <<- solve
        getsolve <- function () m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve 
##the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cache data")
                return (m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}    ## Return a matrix that is the inverse of 'x'
}
