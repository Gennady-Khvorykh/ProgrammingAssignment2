## Data Science Specialization. Coursera
## Course: R programming
## Programming Assignment 2
## Student: Gennady Khvorykh

# Two functions below calculate the inverse matrix for the given one, 
# taking into account whether such matrix has already been calculated. 
# If so, the invertse matrix is taken from the cache and further computation is skiped. 
# Otherwise, the inverse matrix is calculated and placed in cache. 

# Firstly, a special "matrix" object is created by makeCacheMatrix() function.
# Essentially it allows set and get the inverse matrix to and from cache via 
# 'setInverse' and 'getInverse' functions respectively. 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

# Secondly, the inverse matrix is returned by cacheSolve() function.
# It takes as input a special 'matrix' object created by makeCacheMatrix().
# cacheSolve() function also checks whether the initial matrix is invertible. 
# If the matrix is not invertible, the warning message is given.
   
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Cached inverse matrix is given")
    return(m)
  }
  data <- x$get()
  
  #check whether the matrix is invetrible
  invertible <- class(try(solve(data),silent=T))=="matrix"
  if (invertible) {
    m <- solve(data, ...)
    x$setInverse(m)
    m
  }
  else {
    message("Matrix is singular. The invertible does not exist!")
  }
  
}
