## Put comments here that give an overall description of what your
## functions do

## This function creates the special "matrix" that can cache its iverse.

makeCacheMatrix <- function(x = matrix()) {
### This function creates a special "matrix" object that can cache its inverse.
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function() x
	setinverted <- function(solve) m <<- solve
	getinverted <- function() m
	list(set = set, get = get,
		setinverted = setinverted,
		getinverted = getinverted)
}


## Function to do the actual inverse computaton of the above mentioned special matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

### This function computes the inverse of the special "matrix" 
### returned by makeCacheMatrix above. If the inverse has already 
### been calculated (and the matrix has not changed), then 
### cacheSolve should retrieve the inverse from the cache.

	m <- x$getinverted()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverted(m)
	m
}
