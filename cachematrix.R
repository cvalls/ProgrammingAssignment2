## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
## this is the inverted matrix cache.
	m <- NULL

	## without this set works, but Original is created in another env
	original <- NULL

	## this set does not be usefull for our goal.
	## it clears the m matrix cache variable and creates a new x variable 
	## On calling environment, where stores its parameter. set is useful 
	## for change object information
      set <- function(y) 
	{
		print ("storing original matrix on a separate original variable.")
	      original<<- y

		print ("cleaning inverted matrix")
            m <<- NULL
      }
	
	## recover matrix value.
      get <- function() xparameter 
      
	## stores inverted matrix on inverted matrix cache
	setsolve <- function(invmatrix) m <<- invmatrix

	## recovers inverted matrix cache
      getsolve <- function() m

	## this is like an object interface.
	list( set = set, 
		get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
	## this m is an m of calling environment. We can change its name 
      mToReturn <- x$getsolve()

	## if there is no cache, solve and store it
      if ( is.null(mToReturn) ) 
	{
		## matrix value is contained by the x object. We can recover it on data variable
           	data <- x$get()
	     	mToReturn <- solve(data, ...)
		
		## once inverted the matrix is stored
	      x$setsolve(mToReturn)
      }
	else	
	{
		message("getting cached data")
	}
	## I prefer to return explicity on a single output. for me it is easyer to read
	return(mToReturn)

}
