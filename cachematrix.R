## Functions makeCacheMatrix() and cacheSolve() 
## used to compute and cache the inverse of a matrix

# makeCacheMatrix()  
# Takes a matrix as its argument and creates a special "matrix" object
# It's output (list of 4 functions) is input to cacheSolve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set=set, get=get,
       setInv=setInv,
       getInv=getInv)
}

# cacheSolve()  
# Takes the "matrix" object from makeCacheMatrix() as its argument
# Computes the inverse of the matrix
# Retrieves inverse from cache if it was already computed and if matrix is unchanged 
cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv    
}
