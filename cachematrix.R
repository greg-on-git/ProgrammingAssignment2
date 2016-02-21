## These two functions will first wrap a matrix in a list along with its
## in, and second use the cached inverse if it is set so it does not need
## to be re-calculated

## makeCacheMatrix will cache the inverse of the matrix passed in

makeCacheMatrix <- function(x = matrix()) {
	## cache the inverse of the matrix passed in
	
	solved <- NULL
	
	## Create functions to get and set the matrix and its inverse
	set <- function(y) {
			x <<- y
			solved <<- NULL
	}
	
	get <- function() x
	setinverse <- function(solve) solved <<- solve
	getinverse <- function() solved
	
	## Return a List with the functions we created above
	list(set = set, 
		 get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)	
}

## cacheSolve will check if the inverse of the matrix object created by 
## makeCacheMatrix has been set and will set it if not

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	inverted <- x$getinverse()
	
	## If the inverse is already cached, return it
	if(!is.null(inverted)) {
			message("getting cached data")
			return(inverted)
	}
	
	## Calculate the inverse and set it in the cache, then return it
	tempmat <- x$get()
	inverted <- solve(tempmat)
	x$setinverse(inverted)
	inverted
}	
