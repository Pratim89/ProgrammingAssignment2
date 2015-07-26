## The following pair of functions computes the inverse of a given matrix and caches the inverse to avoid repeated computations


## This function creates a special "matrix" object that can cache its inverse
## This special "matrix" is nothing but a list of functions that do the following:
## 1. Set the matrix
## 2. Get the matrix
## 3. Set the inverse
## 4. Get the inverse 

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y){
		x <<- y
		i <<- NULL
	}	
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function()i
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	i <- x$getinverse()
	if(!is.null(i)){
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}
