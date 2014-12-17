## When the matrix is large, matrix inversion will be costly. So the
## following two functions will cache the inverse of a matrix rather
## than compute it repeatly. 

## makeCacheMatrix function is creating an objcet that stores the 
## original matrix and the cached inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_matrix <- function(solve) m <<- solve
        get_matrix <- function() m
        list(set = set, get = get,
             set_matrix = set_matrix,
             get_matrix = get_matrix)
}

## cacheSolve function computes the inverse of the "matrix" returned
## by makeCacheMatrix. If the inverse matrix has been cached, then 
## the cached inverse matrix (m) will be returned. If the inverse matrix 
## has not been cached, then the inverse matrix will be computed and 
## cached.

cacheSolve <- function(x, ...) {
        m <- x$get_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_matrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
