## This function creates a special "matrix" object that can cache its inverse.
## then actually computes the inverse of the special "matrix."

makeCacheMatrix <- function(x = matrix()) 
{
  ## This function creates a special "matrix" object that can cache its inverse.
  
  i             <- NULL
  
  set           <- function(y) 
  {
    x           <<- y
    i           <<- NULL
  }
  
  get           <- function() x
  setinverse    <- function(inv) 
  i             <<- inv
  getinverse    <- function() i
  
  list(
    ## Used equals signs here to avoid creating global variables
    set         = set,
    get         = get,
    setinverse  = setinverse,
    getinverse  = getinverse
      )
}


cacheSolve <- function(x, ...) 
{
  ## This function computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix above.
  
  i            <- x$getinverse()
  
  if(!is.null(i)) 
    {
    return(i)
    }
  
  m           <- x$get()
  i           <- solve(m, ...)
  x$setinverse(i)
  i                 ## Returns a matrix that is the inverse of 'x'
  
  
  
}