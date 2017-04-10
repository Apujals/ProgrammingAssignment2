makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}

cacheSolve <- function(x = matrix(), ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse matrix")
    return(inv)
  }
  data <- x$get
  inv <- solve(data)
  x$setInv(inv)
  inv
}
