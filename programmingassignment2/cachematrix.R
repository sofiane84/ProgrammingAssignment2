## Those functions will cache the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        ## function create a list that can cache the inverse of a matrix=x
        
        m <- NULL
        set <- function(y) {
                ## set the value of the matrix
                
                x <<- y
                m <<- NULL
        }
        get <- function() x
        ## get the value of the matrix
        
        setInverse <- function(Inverse) m <<- Inverse
        getInverse <- function() m       
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        ## set/get inverted matrix and store them in a list
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting Inverse data")
                return(m)
        }
        data <- x$get()
        m <- solve(t(data.matrix(data, ...)))
        x$setInverse(m)
        m
}
