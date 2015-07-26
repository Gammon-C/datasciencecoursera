##ProgrammingAssignment2
## function that creates a "special" matrix and that caches the inverse
makeCacheMatrix <- function(x = matrix()) {

 inv <- NULL
 set <- function(y){
      x <<- y
      inv <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) inv <<- inverse
 getinverse <- function() inv
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
      
}

## function that computes the inverse of the special "matrix" returned by
## the function makeCacheMatrix. When the inverse has been calculated,
## and the matrix has not change, then cacheSolve will retrieve the inverse
## from cache.

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if(!is.null(inv)) {
          message("... getting cached inverse matrix")
          return (inv)
     } else {
     inv <- solve(x$get())
     x$setinverse(inv)
     return(inv)
     }
}

