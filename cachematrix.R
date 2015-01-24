## there is a couple of functions. Between both and a sample script it is possible to 
## make a matrix inversion operation, using a stored data when we need the inverted matrix.

## before continue i have change var names by other more clear for me.

## this funcion acts like an object. Public members are contained in the returned list.
## This objetct is like a wrapper of the matrix we want to invert, so when we work with a matrix
## we will work with a this kind of object. Matrix to invert is the only parameter of the function
## This object contains, data and functions. Only functions contained in the returned list will be called
## it could be easily extended for other matrix operations. 
makeCacheMatrix <- function(xparameter  = matrix()) {
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


## Second function gets a makeCacheMatrix object and calls its internals membes to 
## compute and store inverted matrix. Matrix data, of course, was defined on makeCacheMatrix 
## construction (first time we called) and it was stored inside m variable of the object
## Finally Returns a matrix that is the inverse of 'x'
## it is possible to set other params througth ... argument, but this version ignores them
cacheSolve <- function(x, ...) {        
        
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