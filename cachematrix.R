## ##CacheMatrix function generates a list containing functions to set,get,setinverse,getinverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
        x <<- y
       inv <<- NULL
       }
       get <- function() x
       setinv <- function(inverse) inv <<- inverse
       getinv <- function() inv
      list(set=set, get=get, setinv=setinv, getinv=getinv)
  }


## cacheSolve function return the inverse of the given data.
##It checks if the inverse was calculated already. If calculated,
##the inverse value is obtained from the cache , if the inverse does not
##exist then ,the value is calculated .

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
     }
     data <- x$get()
     inv <- solve(data)
     x$setinv(inv)
     inv
}
