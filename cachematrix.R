
## makeCacheMatrix accepts matrix as as input and returns four functions 
## these four functions are stored in the calling variable

makeCacheMatrix <- function(x = matrix()) {                       # initial matrix sent as x
      inv <- NULL                                                 # setting initial inverse matrix to NULL
      set <- function(y) {                          
            x <<- y                                               # setting new matrix sent as y
            inv <<- NULL                                          # setting inverse matrix to NULL globally so that 
      }                                                           # new inverse should be calculated for new matrix
      get <- function() x                                         # return value of matrix stored as x
      setInverse <- function(inv_new) inv <<- inv_new             # setting newly calculated inverse matrix globally
      getInverse <- function() inv                                # returning inverse matrix which is available globally, serving as cached
      list(set = set, get = get,                                  # names are given to the return functions so that they can be called with $
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve function will first check if inverse of matrix exists
## returns the cached matrix if it exists, else will calculate new inverse matrix
## and set it as new global value by calling setInverse function

cacheSolve <- function(x, ...) {
      inv <- x$getInverse()                                       # get existing value of inverse matrix   
      if(!is.null(inv)) {                                         # check if inverse matrix is not NULL
            message("getting cached matrix")
            return(inv)                                           # return cached inverse matrix if not NULL
      }
      data <- x$get()                                             # store matrix as data
      inv <- solve(data, ...)                                     # calculate inverse of the matrix
      x$setInverse(inv)                                           # set the calculated inverse matrix
      inv
}



mymatrix <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
mymatrix
aMatrix <- makeCacheMatrix(mymatrix)
aMatrix$get()                  # retrieve the value of matrix
aMatrix$getInverse()           # retrieve the value of inverse, which should be NULL
secondmatrix <- matrix(c(4,5,6,7), nrow = 2, ncol = 2)
aMatrix$set(secondmatrix)      # reset value with a new matrix
cacheSolve(aMatrix)            # inverse calculated of second matrix
aMatrix$getInverse()           # retrieve it directly, now that it has been cached
