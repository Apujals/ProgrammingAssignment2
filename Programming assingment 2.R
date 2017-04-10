## first, for making quicklier the test, create a simple matrix 2x2
x <- matrix(1:4, 2,  2)


makeCacheMatrix <- function(x = matrix()) {

  ## nothing is cached so set it to NULL
  inv <- NULL
  
  ## storing the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## return the matrix
  get <- function() x
  
  ## caching  
  setInverse <- function(inverse) inv <<- inverse
  
  ## getting the cached value
  getInverse <- function() inv
  
  ## list of the functions "names = functions"
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- function(A, ...) {
 
  ## gets the matrix and store it in "inv"
  inv <- A$getInverse()
  
  #if found --> not NULL; and return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## not found --> createing inverse matrix with "solve" function
  mat <- A$get()
  inv <- solve(mat, ...)
  
  ## returning inverse matrix
  A$setInverse(inv)
  inv
}


## Example:

## > m <- makeCacheMatrix(x)   ----- setting the cached matrix in "m" 

## > m$get()                   ----- getting the matrix cached
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4


## > cacheSolve(m)             ----- callin "m" in "cacheSolve" function to get the inverse matrix
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
