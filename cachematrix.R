## makeCacheMatrix fuction has two variables x (matrix) and inv its inverse
## when we make a cached matrix using makeCacheMatrix it simply provides us matrix
## and related function to set, get matrix and inverse
## when we create this cachedMatrix inverse is not calculated at this time


makeCacheMatrix <- function(x ) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invv) inv <<- invv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse when has not already been calculated
## for every matrix provided it checks whether the inverse exists i.e. its not null
## if inverse exists it simply takes that value
## otherwise it calculates and stores inverse, so that iverse could be used on later 
## access

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  message("calculating inverse ")
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
