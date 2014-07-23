## Put comments here that give an overall description of what your
## functions do

## creates a "CacheMatrix", which is a list that houses functions to handle the
## input matrix

makeCacheMatrix <- function(x = matrix()) {
      inverse <- NULL
      
      set <- function(y){
            x <<- y
            inverse <<- NULL
      }
      
      get <- function() x
      setinverse <- function(updatedInverse) inverse <<- updatedInverse
      getinverse <- function() inverse
      
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of the input matrix, either from chache or by using solve()

cacheSolve <- function(x, ...) {
        checkInverse <- x$getinverse()
        
        if(!is.null(checkInverse)){
              message("Getting cached data")
              return(m)
        }
        
        matrix <- x$get()
        newInverse <- solve(matrix)
        x$setinverse(newInverse)
        
}
