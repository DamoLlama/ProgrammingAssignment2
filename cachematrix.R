## makeCacheMatrix() creates an object to use in cacheSolve() to 
## check if the inverse of a matrix is in cache and use that, or create
## and store the inverse matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##create matrix object
        im <- NULL
        ##create x and im in global environment, with x passed matrix
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        ##function to return matrix passed to this function
        get <- function() x
       
        ##function to set cached value for inverse matrix 
        setinvmatrix <- function(z) im <<- z
        
        ##function to return cached matrix, or null if not cached
        getmatrix <- function() im 
        
        ##output as a list of functions
        list(set = set, get = get, setinvmatrix = setinvmatrix, getmatrix = getmatrix)
}


## function that takes an object created by function makeCacheMatrix and a matrix
## to be inverted.
## Checks to see if the inverse matrix is stored in  cache and 
## if not creates, stores new inverse matrix in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##get current value of im - should be a matrix is cached or null if not
        im <- x$getmatrix()
        ##check if im in not null and therefore cached and return im
        if(!is.null(im)){
                ##message("getting cached data")
                return(im)
        }
        
        ##get matrix passed as argument to this function
        dataset <- x$get()
        
        ##create inverse of matrix
        im <- solve(dataset)
        
        ##set the cache to store inverse matrix 
        x$setinvmatrix(im)
}
