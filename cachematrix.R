## The two functions defined below illustrate how to store a matrix and it's inverse
## in makeCacheMatrix and then to use this function from cacheSolve so as to avoid
## calculating the inverse of a matrix unnecessarily.

## makeCacheMatrix stores both the actual matrix and, once calculated, the inverse of that
## matrix.  makeCacheMatrix does not calculate the inverse but provides a place to store
## the data persistently so that multiple calls to cacheSolve return the inverse without
## having to calculate it if it has already been calculated.  makeCacheMatrix returns a
## set of functions 'get', 'set', 'getinverse' and 'setinverse' which enable the calling
## function (cacheSolve) to manipulate the data in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {

        # initialise 'inversematix' to be NULL so that we delete any previous 
        # values.  'inversematrix' is only used locally. 
        inversematrix <- NULL
        
        # create the 'set' function which takes 'y' as input and copies it into 
        # a parent-environment variable 'x' and makes sure that the parent-environment
        # variable 'inversematrix' is null to prevent the return of incorrect cached data.
        # Note that 'x' and 'inversematrix' are not actually set here, that only happens
        # when 'set' actually gets called instead of defined.
        set <- function(y) {
            x <<- y
            inversematrix <<- NULL
        }
        
        # create the 'get' function which just returns the local variable 'x' which was 
        # defined in the 'set' function.
        get <- function() x
        
        # create the 'setinverse' function which takes 'y' as the input and then pushes
        # this to parent-environment variable 'inversematrix' which not only stores the
        # cached inverse matrix but is also used to indicate if the inverse needs to
        # be calculated or not.
        setinverse <- function(y) inversematrix <<- y
        
        # create the 'getinverse' function which retrieves the value previously pushed
        # in 'inversematrix' by 'setinverse'
        getinverse <- function() inversematrix
        
        # create a list containing the names of all the functions created.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve takes a previously created object of type makeCacheMatrix as input
## and works out whether to return the cached result stored in makeCacheMatrix or
## to calculate the inverse of the data in makeCacheMatrix and to store the resulting
## inverse matrix back into the makeCacheMatrix object.

cacheSolve <- function(x, ...) {

        # get the cached inversematrix if it has already been calculated.
        # If it hasn't this will return a null value.
        inversematrix <- x$getinverse()
        
        # Check that the cached inversematrix is not null and then return it's value
        # thus avoiding a recalculation of the inverse of a matrix.
        if(!is.null(inversematrix)) {
            message("Returning a cached inverse matrix")
            return(inversematrix)
        }
        
        # call the 'get' function defined in makeCacheMatrix to retrieve the matrix in
        # question and to put this into data.
        data <- x$get()
        
        # calculate the inverse of 'data' passing into solve any of the extra parameters
        # passed into cacheSolve.
        inversematrix <- solve(data, ...)
        
        # now that we have a an inverse matrix, use 'setinverse' to cache it in
        # makeCacheMatrix.
        x$setinverse(inversematrix)
        
        # return the inverse matrix calculated.
        message("Returning a calculated inverse matrix")
        inversematrix
}
