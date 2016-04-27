## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and creates functions associated with it.
## These function cache the matrix and update as necessary. 
## No mathematical calculations occur here.

makeCacheMatrix <- function(x = matrix()) {

	## Initialises minv - the inverse matrix.
	## Or re-initialises it if the makeCacheMatrix function is being run again.
	minv <- NULL
	
	## Set lets you redefine your original matrix. 
	## Since you have a new matrix, it sets minv to NULL so you can compute it again.
	
	set <- function(y) {
	
		## x is from the global environment and minv is from the parent environment to 
		## what's inside this function, hence '<<-' instead of '<-'. 
		## This changes the above x and minv, instead of creating new local variables 
		## that are only used inside set.
		x <<- y
		minv <<- NULL
		}
	
	##	get is a function without arguments. When called it returns the original x matrix.
	get <- function() x
	
	## If you've calculates an existing inverse (exist_inv), you can pass it to setinv.
	## setinv then assigns the matrix you pass it to be minv (ie, caches it).
	## again, double arrow brackets as minv is not just local for setinv.
	setinv <- function(exist_inv) minv <<- exist_inv
	
	## Requiring no arguments, getinv returns the inverse matrix, minv (ie, cached matrix).
	getinv <- function() minv

	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve returns the inverse of the matrix x.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'. 
		## It either calculates it, or if it's in the cache, it just gets that.
		
		## Gets the value of the inverse from its cache (either NULL or a cached result)
			
		inv <- x$getinv()
		
		## If there is already an inverse, then it returns cached data (and tells you that)
		if (!is.null(inv)) {
			message("Getting cached inverse")
			return(inv)
		}
		
		## If there isn't an inverse, it gets the matrix, solves for its inverse and caches the result
		mat <- x$get()
		inv <- solve(mat, ...)
		
		## Updates cache
		x$setinv(inv)
		
		## Prints m - inverse matrix of x
		inv
}
