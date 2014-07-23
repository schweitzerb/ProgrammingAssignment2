## Put comments here that give an overall description of what your
## functions do

## creates a "CacheMatrix", which is a list that houses functions to handle the
## input matrix

makeCacheMatrix <- function(x = matrix()) {
      
      inverse <- NULL ##assign NULL as inverse has not yet been calcualted
      
      ##function to enable updating the input matrix
      set <- function(y){ 
            x <<- y
            inverse <<- NULL ##reset the inverse since the matrix has changed
      }
      
      ##function to return the matrix 
      get <- function() x
      
      ##function to set the inverse
      setinverse <- function(updatedInverse) inverse <<- updatedInverse
      
      ##function to return the inverse of the input matrix
      getinverse <- function() inverse
      
      list(set = set, get = get, 
           setinverse = setinverse, getinverse = getinverse)
}


## returns the inverse of the input matrix, either from chache or by using 
## solve() to calcualate the inverse

cacheSolve <- function(x, ...) {
      
      ##check to see if the inverse is already cached, return it if true
      checkInverse <- x$getinverse()          
      if(!is.null(checkInverse)){ 
            message("Getting cached data...")
            return(checkInverse)
      }
      
      ##calculate the inverse (will only execute if it wasn't found in cache)
      matrix <- x$get()
      newInverse <- solve(matrix)
      x$setinverse(newInverse)
      newInverse
}


