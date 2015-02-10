## Matrix inversion is usually a costly computation 
## and their may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly.
## This pair of functions allows to caches the inverse of a matrix

##  This function creates a special "matrix", which is really a list containing a function to
##  * set the value of the matrix
##  * get the value of the matrix
##  * set the value of the inversed matrix
##  * get the value of the inversed matrix
makeCacheMatrix <- function(x = matrix()) {

    # cached inversed matrix
    inversedMatrix <- NULL

    # sets real matrix 
    set <- function(y) {
        x <<- y
        # when matrix is set inversed matrix is set to NULL
        inversedMatrix <<- NULL
    }
    
    #gets real matrix
    get <- function() x
    
    #sets cache
    setSolve <- function(solve) inversedMatrix <<- solve
    
    #reads cache
    getSolve <- function() inversedMatrix
    
    #returns list of functions to access matrix and its inversed matrix
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function calculates the inversed matrix of the special "matrix" 
## created with the function makeCacheMatrix. 
## However, it first checks to see if the inversed matrix has already been calculated. 
## If so, it gets the solution from the cache and skips the computation. 
## Otherwise, it calculates the inversed matrix and sets the solution in the cache 
## via the setSolve function.
cacheSolve <- function(x, ...) {
    # check the cache
    inversedMatrix <- x$getSolve()
    if(!is.null(inversedMatrix)) {
        #if cache contains the solution return it
        message("getting cached data")
        return(inversedMatrix)
    }
    #otherwise calculate inversed matrix
    data <- x$get()
    inversedMatrix <- solve(data, ...)
    #and store the solution in cache for future use
    x$setSolve(inversedMatrix)
    
    inversedMatrix
}
