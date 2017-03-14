## The function makeCacheMatrix() creates a special matrix object
## If the matrix inverse has already been calculated, it will instead find it in the cache and return it, 
## and if inverse has not been calculated then it'll calculate it

makeCacheMatrix <- function(x = matrix()) {
  inv_x <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment i.e. different from the current environment. 
    x <<- y
    inv_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inv_x <<-inverse
  getinverse <- function() inv_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve() returns the inverse of a matrix A created with makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it, while if not, it computes, caches, and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_x <- x$getinverse()
  
  # if the inverse has already been calculated
  if (!is.null(inv_x)) {
    # get it from the cache and skips the computation. 
    message("getting cached inverse matrix")
    return(inv_x)
  }
  # otherwise, calculate the inverse
  else {
    inv_x <- solve(x$get())
    x$setinverse(inv_x)
    return(inv_x)
  }
}



a <- diag(7,5)
a
CachedMarix <- makeCacheMatrix(a)
b <-cacheSolve(CachedMarix)
a*b
