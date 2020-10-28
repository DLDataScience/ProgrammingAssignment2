## The functions below assume that they are dealing with an invertible matrix.
## The purpose of these functions is to cache the inverse of some matrix, x, and to retrieve it
## if the calculation has already been cached or to execute it if the calculation has not yet been cached.


## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
   
   # initiate inv with a default NULL value
   inv <- NULL
   
   # initiate `set()` as a function to reassign x using <<- and R scoping rules
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   
   # initiate `get()` as a function to retrieve the current value of x
   get <- function() x
   
   # initiate `setinv()` as a function to reassign inv using <<- and R scoping rules
   setinv <- function(i) inv <<- i
   
   # initiate `getinv()` as a function to retrieve the current inverse of x
   getinv <- function() inv
   
   return(list(set = set, get = get,
               setinv = setinv, getinv = getinv))

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   
   if(!is.null(inv)) {
      message('getting cached data')
      return(inv)
   }
   
   mat <- x$get()
   inv <- solve(mat)
   x$setinv(inv)
   
   return(inv)
}

