## This R code is produced to meet the requirement
## for Programming Assignment 2 in the R Programming
## Course.

## Two functions (makeCacheMatrix and cacheSolve) 
## are used to create a special "matrix" object 
## and stores the input matrix and caches its
## the output (the inverse of the matrix).

## makeCacheMatrix is a helper function used to
## assign the matrix object and its inverse to 
# its cache and retreive them.

## cacheSolve is a function that returns the
## inverse of the input matrix, either as a
## first-time calculated value or cached result.

## Note that these two functions have to be 
## called in sequence in order to obtain the 
## correct result. That is, in order for
## cacheSolve to return the correct result, 
## makeCacheMatrix has to be called first.


# This function creates setters and getters
# (aka accessors) for use in the matrix
# inverse operation as well as two internal
# storage objects for the input matrix and 
# the calculated result (x and inv). It then 
# associates the setters and getters with the 
# matrix object using the list function.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                         # Initialize the inv object
    set <- function(y) {                                # Define how the input matrix is stored
        x <<- y                                         # Stores the computed value
        inv <<- NULL                                    # Initialize the inv object
    }
    get <- function() x                                 # Define how the input matrix is retreived
    setinverse <- function(inverse) inv <<- inverse     # Define how inv is stored
    getinverse <- function() inv                        # Define how inv is retrieved
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) # Associate the setters and getters for x and inv
}

# This function returns the inverse of a
# matrix object. Each time this function is 
# called (with x being the input matrix), it
# will access the cache of the matrix object
# first and see if its inverse is there
# (x$getinverse). If the cached result is 
# already there (!is.null()) it will be returned 
# (return(inv)). Otherwise it will access the 
# matrix object (x$get()) and calculate its 
# inverse using solve() and store the
# result in the cache (x$setinverse(inv)).
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()                       # Retreives the inverse for the input matrix, x
    if(!is.null(inv)) {                         # Checks to see if the cache inverse object exists
        message("getting cached data.")         # Prompts the user if the value is retreived from cache
        return(inv)                             # Retreives the cached inverse
    }
    data <- x$get()                             # Stores the input matrix in cache
    inv <- solve(data)                          # Performs the actual matrix inverse operation
    x$setinverse(inv)                           # Stored the freshly calculated result
    inv                                         # Displays the calculated result
}
