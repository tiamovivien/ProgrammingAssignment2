## These functions are going to create a special "matrix" object 
## which can cache its inverse. The other time you want to aquire
## the inverse, you do not need to recalculate it again, just take
## get it easily. It firstly uses the function "makeCacheMatrix" to 
## create the "matrix" object, which is really a list of functions 
## to set / get the matrix as well as its invers, secondly it use 
## function cacheSolve to calculate the inverse of the matrix just ONCE,
## and other time it  will check out whether the inverse has been 
## calculated before, if so the inverse will be directly return to the 
## first function.

## The function "makeCacheMatrix" need an input of numerical invertible
## matrix and return a "matrix" object with its inverse containing in it. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(crt_inv) inv <<- crt_inv
	getinv <- function() inv
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv) 
}


## The function "cachSolve" is going to calculate the inverse of the matrix just
## ONLY ONCE. Every time it is called, it will check out that whether the inverse
## has already been calculated before, if so it will be directly return, otherwise
## it will be calculated and cached.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if (!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	inv
}
