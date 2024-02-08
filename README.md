# avi.R
To solve this programming assignment, you need to create two functions: makeCacheMatrix and cacheSolve.
# Define a function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cache
    cache <- NULL
    
    # Define a function to set the matrix
    set <- function(y) {
        x <<- y
        cache <<- NULL
    }
    
    # Define a function to get the matrix
    get <- function() x
    
    # Define a function to compute and cache the inverse of the matrix
    setInverse <- function(inverse) cache <<- inverse
    
    # Define a function to get the cached inverse of the matrix
    getInverse <- function() cache
    
    # Return a list of functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Define a function to compute the inverse of a matrix (cached if possible)
cacheSolve <- function(x, ...) {
    # Check if the inverse is already cached
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    # Compute the inverse using solve() function
    data <- x$get()
    inverse <- solve(data, ...)
    
    # Cache the inverse
    x$setInverse(inverse)
    
    # Return the inverse
    inverse
}
