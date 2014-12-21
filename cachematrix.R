## The functions below calculate or recall the inverse of a matrix.  In order to save
## computing time, the second function "cacheSolve" checks to see if the 
## inverse of the matrix under evaluation has already been calculated.  If 
## the inverse has already been calculated, the function will return the 
## previously calculated value rather than performing the operation again.

## This first function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

## Create a local varable "m" with a NULL value as a location to store the matrix inverse
  m <- NULL                                 
  
  set <- function(y) {      #defines "set" as a function of y 
    x <<- y                 #sets y as the value for "X" in the containing environment
    m <<- NULL              #sets "m" in the containing environment to a NULL value
    }
  get <- function() x       #defines "get" as an empty function that returns matrix "x" as it's expression    
  
## setInverse below is defined as a function that uses "inverse" as it's argument
## and returns "m" as it's expression.
## "inverse" is defined as the value of the global variable "m"
  setInverse <- function(inverse) m <<- inverse 

## getInverse is defined as an empty function that returns the expression "m"
  getInverse <- function() m

## The list function below compiles the results of the defined functions above in a list.
  list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse) 
}


## The function "cacheSolve" computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve function will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  m <- x$getInverse()       # Set the "getInverse" value of matrix "x" into m

  ## Check to see if the value is NOT NULL.   
  ## If there is a value, print the message and return the value of "m".
  if(!is.null(m)) {                            
    message("Retrieving previously calculated matrix inverse")
    return(m)
  }
   
  data <- x$get()           # Set the "get" value of matrix "x" into the data variable
  m <- solve(data, ...)     # Solve for the inverse of the matrix containted in data and store as m
  x$setInverse(m)           # Sets the "setInverse" value in matrix "x" to m computed above as m
  m                         # Prints the value of m (the solved matrix inverse)
}
  

