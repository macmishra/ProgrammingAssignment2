## The function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of input matrix
## get the value of the inverse of input matrix
makeCacheMatrix <- function(x = matrix()) {
  # It's a new matrix object, set inverse to be undefined
  inv <- NULL
  
  # Define value for the matrix part, set inverse to be undefined
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Return matrix when accessed
  get <- function() x
  
  # Whenever 'solve' function is called on, set inverse value
  setinv <- function(solve) inv <<- solve
  
  # Return inverse matrix when accessed
  getinv <- function() inv
  
  # Define list of functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function cacheSolve takes input a matrix and 
cacheSolve <- function(x = matrix(), ...) {
  ## Access inverse of input matrix
  inv <- x$getinv()
  ## Check if input matrix has an inverse defined/ stored from previous operations
  if(!is.null(inv)) {
    ## If already has inverse calculated, return it
    message("getting cached data")
    return(inv)
  }
  
  ## Get actual matrix data out of input special matrix (defined using makeCacheMatrix)
  data <- x$get()
  
  ## Calculate inverse
  inv <- solve(data, ...)
  ## Cache inverse matrix with the input matrix data for future use
  x$setinv(inv)
  
  ## Return inverse of the cache matrix
  inv
}


## Testing
## > source("ProgrammingAssignment2/cachematrix.R")
## > matrix1 <- makeCacheMatrix(matrix(1:4, 2))
## > inverse1 <- cacheSolve(matrix1)
##
## check if the inverse os correct
## > matrix1$get() %*% inverse1
## [,1] [,2]
## [1,]    1    0
## [2,]    0    1
## 
## generate inverse once again to check
## > inverse2 <- cacheSolve(matrix1)
## getting cached data