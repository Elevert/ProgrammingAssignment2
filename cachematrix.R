#Matrix inversion is usually a costly computation and 
#there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly. 
#The following pair of functions can be used as alternative to cache the inverse of a matrix.

 
 
# makeCacheMatrix creates a list containing a function to 
# 1. set the value of the matrix 
# 2. get the value of the matrix 
# 3. set the value of inverse of the matrix 
# 4. get the value of inverse of the matrix 

 makeCacheMatrix <- function(x = matrix()) { 
     inv.m <- NULL 
     set <- function(y) { 
         x <<- y 
         inv.m <<- NULL 
     } 
     get <- function() x 
     setinverse <- function(inverse) inv.m <<- inverse 
     getinverse <- function() inv.m 
     list(set=set, get=get, 
		setinverse=setinverse, 
		getinverse=getinverse) 
 } 

 
# The following function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above.
# If the inverse has already been calculated (and the matrix has not changed), 
#then  cacheSolve skips the computation and retrieves the inverse from the cache. 
# If not, it computes the inverse with the  solve  function in R., 
#setting the value in the cache via setinverse function created in the first function. 
# The matrix is assumed always invertible for this assignment.


 cacheSolve <- function(x, ...) { 
     inv.m <- x$getinverse() 
     if(!is.null(inv.m)) { 
         message("getting cached data") 
         return(inv.m) 
     } 
     data <- x$get() 
     inv.m <- solve(data) 
     x$setinverse(inv.m) 
     inv.m
 } 
