## Creates a special "matrix" object that can cache its inverse.
 ## Returns a list of four functions to set the matrix value, get the matrix value, set the inverse value, get the inverse value.
 makeCacheMatrix <- function(x = matrix()) {

         ## initialise i to null
         i <- NULL

         ## create a function that sets the matrix value, and removes any previously cached inverse value
         set <- function(y) {
                 x <<- y
                 i <<- NULL
         }

         ## create a function that returns the matrix value
         get <- function() x

         ## create function that sets the cached inverse value
         setinverse <- function(inverse) i <<- inverse

         ## create a function that returns the cached inverse value
         getinverse <- function() i

         ## create a list of four functions that are returned.
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)

 }


 ## Returns a matrix that is the inverse of 'x'
 ## It first checks to see if the inverse has already been calculated.
 ## If so, it gets the inverse from the cache and skips the computation.
 cacheSolve <- function(x, ...) {

         ## Try to get the cached value
         i <- x$getinverse()

         if(!is.null(i)) {
                 ## inverse was cached, so return the cached value without calculating.
                 message("getting cached data")
                 return(i)
         }

         ## Value was not cached, so calculate the inverse
         data <- x$get() ## assign the matrix value into 'data'

         ## calculate the inverse of the matrix and store in 'i'
         i <- solve(data, ...)

         ## cache the result 'i' for future use
         x$setinverse(i)

         ## return the calculated inverse
         i
 }


 ## Test on empty matrix
 q <- makeCacheMatrix(matrix())


 cacheSolve(q)

 ## Second call should return cached value
 cacheSolve(q)