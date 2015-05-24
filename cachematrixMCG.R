## cachematrix is used to create a special object that stores a 
## matrix vector and cache's its inverse

## makeCacheMatrix creates a special object, which is actually a
## list containing a function

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) { ## sets the value of the matrix
    x <<- y
    v <<- NULL
  }
  get <- function() x ## gets the value of the matrix
  setInverse <- function(inverse) v <<- inverse ## sets the value 
                                                ## of the inverse
  getInverse <- function() v ## gets the value of the inverse
  list(set = set, get = get, ## list contains function components
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special matrix
## created by makeCacheMatrix

cacheSolve <- function(x, ...) {
  v <- x$getInverse()
  if(!is.null(v)) { ## checks whether inverse has already been 
                    ## calculated and is cached
    message("getting cached data")
    return(v) ## if so, gets the inverse from the cache
  }
  data <- x$get() ## if inverse has not already been calculated, 
                  ## the function calculates the inverse
  v <- solve(data, ...)
  x$setInverse(v) ## sets the value of the inverse in the cache
  v
        ## Returns a matrix v that is the inverse of x
}
