## This file is to reduce how long it takes to 
## run time-consuming computations involving matrix
## inverse computations by using cache-ing.

## This function takes a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function returns the inverse of a matrix.
## It checks if the inverse is cached, and if not
## it computes the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if (!is.null(m)) {
    	message("getting cached data")
    	return(m)
    }
    data <-x$get()
    m<- solve(data, ...)
    x$setinverse(m)
    m
}

