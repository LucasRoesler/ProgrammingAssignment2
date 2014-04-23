## makeCacheMatrix creates a matrix like object capable of caching
##     the results of solve.
## cacheSolve will solve and cache the results in the CacheMatrix
##     object.

#####
## makeCacheMatrix creates a matrix like object that is capable of 
## caching its inverse.
##
## Arguments
##  x        a matrix

makeCacheMatrix <- function(x = matrix()) {
        # i is the name of the cached inverse
        i <- NULL
        
        # define the matrix getter and setter
        set <- function(y) {
          # if the matrix is changed, make sure to clear 
          # the cached inverse.
          x <<- y
          i <<- NULL
        }
        get <- function() x
        
        # define the inverse getter and setter
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        # return the list object
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#####
## Find the inverse of a CacheMatrix using solve(), if the 
## the inverse has already been solved, we returned the cached
## value.  solve can be used to solve the equation a %*% x  = b
## for x, where b can be either a vector or a matrix.

## Arguments
##  x        a CacheMatrix, i.e. the result of makeCacheMatrix
##  ...      the arguments of solve(), from the solve 
##           documentation these can be:
##           b    a numeric or complex vector or matrix giving the 
##                right-hand side(s) of the linear system. If missing, 
##                b is taken to be an identity matrix and solve will 
##                return the inverse of x.
##           tol  the tolerance for detecting linear dependencies in 
##                the columns of a. The default is .Machine$double.eps. 
##                Not currently used with complex matrices a.
##  for other details see solve.
cacheSolve <- function(x, ...) {
        # if no additional arguments are passed to cacheSolve, 
        # then we try to get the inverse from the cace.
        dots <- list(...)
        if(length(dots)==0){
            i <- x$getinverse()
            # if the cached inverse exists, return it 
            # immediately. 
            if(!is.null(i)) {
              message("inverse from cached data")
              return(i)
            }    
        }
        
        # if there are additional arguments, or the cache is not 
        # yet set, the rest of the function will run.  It will 
        # run solve using the matrix in x and if there are no
        # additional arguments, cache the result. 
        data <- x$get()
        i <- solve(data, ...)
        
        if(length(dots)==0){
            # only cache the inverse when solve is not passed 
            # any additional arguments
            x$setinverse(i)
        }
        
        # return the result, if there are 
        i
}
