## Asignment Cachine the inverse of a matrix
## Looked at youtube tutorials to see what we are doing with inverse matrix functions
## Looked with str() function about the syntax
## Replaced the mean function for the inverse function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

## Did the testing and the math with these numbers. Did not do the math for a 3x3, too complicated ;-):
## Underneath the results
## x = rbind(c(1, 2), c(3, 4))
## m = makeCacheMatrix(x)
## m$get()
## [,1] [,2]
## [1,]    1    2
## [2,]    3    4

## cacheSolve(m)
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## > cacheSolve(m)
## getting cached data.
## [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
