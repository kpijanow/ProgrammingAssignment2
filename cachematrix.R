## The two functions are for caching inverse of the matrix

## this function makes an a matrix that can store is inverse as a cache

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}

	get <- function() x
	getinverse <- function() inverse
	setinverse <- function(inv) inverse <<- inv
	list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## this function caches the inverse of the matrix only if it was not prevously computed

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse))
	{
		message("get cached data")
		return(inverse)
	}

	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}
