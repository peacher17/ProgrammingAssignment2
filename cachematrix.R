## makeCacheMatrix creates and returns a list of functions
## used by cacheSolve to get or set the inverted matrix in cache

makeCacheMatrix <- function(x = matrix()) {
# sets the value of M to NULL
	m <- NULL
# create the matrix in the working environment
        set <- function(y) {
# use `<<-` to assign a value to an object in an environment 
# different from the current environment. 
            x <<- y
	    m <<- NULL
	}
	get <- function() x
# invert the matrix and store in cache
	setInverse <- function(inverse) m <<- inverse
# get the inverted matrix from cache
	getInverse <- function() m
# return the created functions to the working environment
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve calcluates the inverse of the matrix created in makeCacheMatrix
## If the inverted matrix does not exist in cache,
## it it created in the working environment and it's inverted value
## is stored in cache

cacheSolve <- function(x, ...) {
# attempt to get the inverse of the original matrix if it has already been calculated
  	m <- x$getInverse()
# if the inverse has already been calculated
	if(!is.null(m)) {
# get it from the cache and skip the computation.
	    message("getting cached data")
# display matrix in console
            return(m)
	}
# otherwise, calculates the inverse
	data <- x$get()
	m <- solve(data, ...)
# sets the value of the inverse in the cache via the setInverse function.
	x$setInverse(m)
	m
}
