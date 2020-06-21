## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix function takes in a matrix as an input and creates a
## special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                    # Initialize variable m which will store the inverse of x
  set <- function(y) {         # defining set function if we want to edit the value of x
    x <<- y
    m <<- NULL
  }
  get <- function() x          # defining get function to return value of x
  setInverse <- function(inverse) m <<- inverse   # If we want to set the inverse of x
  getInverse <- function() m   # Function for Getting the inverse of X
  list(set = set, get = get,   # Output of the function
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

# The cacheSolve function takes in the output of the makeCacheMatrix function as the input
# It checks if the inverse of the entered matrix exists in cache
# If it exists, it returns the cahced inverse
# If not, then it computes the inverse

cacheSolve <- function(x, ...) {    # Take input x that is output of makeCacheMatrix 
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse() # getting inverse of x
  if(!is.null(m)) {   # Checking if inverse is not null(i.e. inverse exists)
    message("getting cached data")
    return(m)       # return the cached inverse
  }
  data <- x$get()     # If inverse does not exist, set the value of matrix in variable data
  m    <- solve(data , ...)   # Get inverse of the matrix data
  x$setInverse(m)     # Store this inverse in the cached data
  m                   # Return the inverse
}