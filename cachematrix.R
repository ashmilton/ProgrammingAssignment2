## The purpose of these functions is to calculate and cache the inverse of a
## matrix. This will make future calculations run faster because the inverse can
## be retrieved from the cache instead of being re-calculated each time.

## makeCacheMatrix() sets and gets a matrix, then sets and gets the inverse
## of the matrix. It then returns a list of these functions.

makeCacheMatrix <- function(x = matrix()) {   
  m <- NULL
  set <- function(y) {                        
    x <<- y                                                         
    m <<- NULL                                
  }
  get <- function() x                         
  setsolve <- function(solve) m <<- solve     
  getsolve <- function() m                    
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve takes x <- makeCacheMatrix(x) as an argument. It checks to see if
## the inverse of the matrix is cached. If so, it returns the cached inverse.
## If the inverse is not cached, it calculates and returns it.

cacheSolve <- function(x, ...) {             
     m <- x$getsolve()                    
     if(!is.null(m)) {                    
          message("getting cached data")
          return(m)                       
      }
     data <- x$get()                      
     m <- solve(data, ...)                
      x$setsolve(m)
      m
  }
