## This functions are used to calculate the inverse of a matrix
## And the inverse of the matrix is cached to avoid recaculate it
## if the matrix does not change

## This function creates a matrix object that can cache its inverse
## set(y) function to set the matrix
## get() function to retrieve the matrix
## setinverse(inverse) function to set the inverse of the matrix
## getinverse() function to retrieve the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the matrix 
## The inverse of matrix is cached to avoid recalculate, so if the 
## matrix does not change the results comes from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data");
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
