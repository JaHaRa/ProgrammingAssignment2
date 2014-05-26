
## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        save <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        savecache <- function(matrix) m <<- matrix
        getcache <- function() m
        list(save = save, get = get,
                    savecache = savecache,
                    getcache = getcache)
}

##This function computes the inverse of the matrix returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) { ## Return a matrix that is the inverse of 'x'
        m <- x$getcache()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$savecache(m)
        m
}
