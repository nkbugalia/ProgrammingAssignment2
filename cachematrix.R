## The function makeCacheMatrix creates a matrix object to cache 
## which can then be retrieved if needed by cacheSolve function, which
## otherwise calculates and returns the inverse of a given matrix.


#Stores inverse of matrix in cache, if exists returns it to cacheSolve(),
## else stores it in cache.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  get <- function() x 
  setMatInv <- function(matrixinverse) inverse <<- matrixinverse 
  getMatInv <- function() inverse
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
}


## Return a matrix that is the inverse of 'x'.
##If it already exists in cache, it retrives, else solves for it.

cacheSolve <- function(x, ...) { 
  inverse <- x$getMatInv()
  
  if(!is.null(inverse)) { 
    message("Getting already cached matrix inverse!")
    return() 
  }
  matrix <- x$get() 
  inverse <- solve(matrix) 
  x$setMatInv(inverse)
  inverse 
}


## used the example code given with assignment to frame these functions!