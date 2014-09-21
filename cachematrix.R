## Put comments here that give an overall description of what your
## functions do
#The purpose of these two functions is the solve for the inverse of passed in 
#matrices, and cache the results


## Write a short comment describing this function
#makeCacheMatrix - this function is a parent function of 4 functions whose
#purposes are to cache solved inverse matrices

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get, setinverse = setinverse,
           getinverse = getinverse)
      
}

## Write a short comment describing this function
# cacheSolve - the purpose of this function is to check if a matrix has already
# been inverted, and, if not, solve for the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached inverse matrix")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      
      i
      
}
