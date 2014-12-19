## Put comments here that give an overall description of what your
## functions do

##  This function creates a special "matrix" object that 
##can cache its inverse.
## not actually working with a matrix, but with a list

makeCacheMatrix<- function(x = list("col1"=c(),"col2"=c(),"col3"=c())) {   
    inv <- NULL
    set <- function(y) {
        x <<- y
        ## superassign inv to calculate to null
        inv <<- NULL
    }
    ## return the original matrix
    get <- function() x
    ## the inverse of the matrix is stored in setinverse
    setinverse <- function(solve) inv <<- solve
    ## return the cached value to cacheSolve()
    getinverse <- function() inv
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special "matrix" 
## returned by makeCachematrix above. 
## If the inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve<- function(x,...) {
    inv <- x$getinverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## this is the else of the if from above
    ## if inv is null than
    data <- x$get()
    ## we have to calculate because we have no value for inv
    inv <- solve(data, ...)
    x$setinverse(inv)
    ## inverse calculated
    inv
    
}
