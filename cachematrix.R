
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
 
  
makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL  #sets inverse_x to Null, providing a default value i Cachecolve has not been run
    set_matrix <- function(y) { # defines a function to set the matrix
      x <<- y  #caches the inputted matrix
      inverse_x <<- NULL  #sets the value of inverse_x to Null
    }
    get_matrix <- function() x  # defines a function to get the matrix
    set_inverse<- function(inverse) inverse_x <<-inverse  # defines a function to set the matrix
    get_inverse <- function() inverse_x  #defines a function to get the inverse
    list(set_matrix = set_matrix, # Builds a list of the above functions
         get_matrix = get_matrix,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# The  function returns the matrix inverse. It first checks if
# the inverse has already been calculated. If it has, it gets the result from cache and skips the
# computation. If not, it computes the inverse, then sets the value in the cache via
# setinverse function.  It finally returns the calculated or retrieved inverse matrix
cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  inverse_x <- x$get_inverse()  #looks for the inverse matric in the cache
  if (!is.null(inverse_x)) {  #if the inverse has already been calculated
  message("getting cached inverse matrix")  # lets user know it's retriving previously cached inverse
  return(inverse_x)
} 
  else {   #If the matrix has not yet been calculated...
  inverse_x <- solve(x$get_matrix())  # Calculates the inverse...
  x$set_inverse(inverse_x) # and puts it in the cache
  return(inverse_x)
  }
}