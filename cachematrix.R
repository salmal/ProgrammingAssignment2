## Put comments here that give an overall description of what your
## functions do
## makecachematrix declares the function to receive the matrix

# It creates additional functions which will be used as methods
# the methods are get the matrix back getmatrix
# store the inverted matrix and cache it
# get the inverted matrix
#create a list object to expose the methods outside

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      # variable to store the inverted matrix
      invmat <- matrix()
      log_val <- FALSE
      getMat <- function() x
      #function to set the inverted matrix
      #mark the logical variable as TRUE
      setInvMat <- function(m) {
            invmat <<- m
            log_val <<- TRUE
      }
      #function to get the inverted matrix
      getInvMat <- function() invmat
      # function return TRUE if matrix is inverted else FALSE
      isInverted <- function() log_val
      #
      #expose the methods through the list for the object created
      list(getMat = getMat,
           setInvMat = setInvMat,
           getInvMat = getInvMat,
           isInverted = isInverted
           )
      
}


## Write a short comment describing this function
# function checks if the inverted matrix is present
# if not present it creates the inversion and caches it
# if present it gets it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      # check if inverted matrix exists
      if(x$isInverted()) {
            message("getting cached data")
            return(x$getInvMat())
      }
      # else get the matrix, invert it, cache it 
      # and return the inverted matrix
      l_mat <- x$getMat()
      inv_m <- solve(l_mat)
      x$setInvMat(inv_m)
      return(inv_m)
      
}
