# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the 
# cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Sample Run:
## > x = rbind(c(1, -1/5), c(-1/5, 1))
## > m = makeCacheMatrix(x)
## > m$get()
## [,1] [,2]
## [1,]  1.0 -0.2
## [2,] -0.2  1.0

## No data has been cached as yet
## > cacheSolve(m)
## [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667

## Cached inversed from first run of cacheSolve
## is retreived without the need to recompute
## > cacheSolve(m)
## getting cached data
## [,1]      [,2]
## [1,] 1.0416667 0.2083333
## [2,] 0.2083333 1.0416667
