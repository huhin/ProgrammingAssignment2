## Caching the inverse of a Matrix
## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function (y){
      x<<-y
      n<<- NULL
  }
  get <- function ()x
  setInverse <- function (Inverse) n<<- Inverse
  getInverse <- function () n
  list (set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        n<- x$getInverse()
        if (!is.null(n)){
            message("getting cached data")
            return (n)
        }
        data <- x$get()
        n<- solve(data, ...)
        x$setInverse(n)
        n        
}