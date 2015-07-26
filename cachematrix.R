
## Function cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
                x_i <- NULL   ## Cache 
		set <- function(y){  ## Reset the matrix and inverse cache
		        x <<- y
		        x_i <<- NULL
		}
		
		get <- function() x   ## Getter to return the matrix
		setInverse <- function(inverse) x_i <<- inverse ## Sets the inverse function
		getInverse <- function() x_i   ## Gets the Inverse
		list(set = set, get = get, setInverse = setInverse,getInverse = getInverse) 
		        ## return for the makeCacheMatrix function

}


## Write a short comment describing this function



cacheSolve <- function(x, ...) {
		## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()       ## get the accessor from cache
		if(!is.null(inverse)) {         ## If value is received the same is returned
			message("from cache")
			return(inverse)
		}
		data <- x$get()                 ## matrix is read from x
		inverse <- solve(data, ...)     ## inverse is calculated
		x$setInverse(inverse)           ## inverse is set in the cache 
		inverse           	        ## inverse is returned
}

