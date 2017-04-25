## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	#assigns NULL to the resulting matrix
	inverse <- NULL
	
	#create function to set value for matrix 
	set <- function(y){
		x <<- y
		inverse <<- NULL
	}
	#create function to get value of matrix
	get <- function() x
	#creates function to set value of inverse of matrix
	setInverse <- function(solve) inverse <<- solve
	#creates function to get value of inverse of matrix
	getInverse <- function() inverse
	list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		inverse <- x$getInverse()
		
		#if inverse has already been calculated, retrieve inverse from cache
		if(!is.null(inverse)){
			message("Retrieving the cached data...")
			return(inverse)
		}
		
		#if inverse has not already been calculated, calculate inverse
		data <- x$get()
		inverse <- solve(data, ...)
		x$setInverse(inverse)
		inverse
}
