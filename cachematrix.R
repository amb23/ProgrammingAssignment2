## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y)
	{
		x <<- y
		xinv <<- NULL
	}
	get <- function()
	{
		return(x)
	}
	
	setInverse <- function(inverse)
	{
		xinv <<- inverse
	}
	getInverse <- function()
	{
		return(xinv)
	}
	
	return(list(set = set, get = get, setInverse = setInverse, getInverse = getInverse))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	xinv <- x$getInverse()
	if(!is.null(xinv))
	{
		message("getting cached data")
		return(xinv)
	}
	
	matdat <- x$get()
	xinv <- solve(matdat, ...)
	x$setInverse(xinv)
	return(xinv)
}
