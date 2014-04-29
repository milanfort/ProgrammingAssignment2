#
# Solution to programming assignment from Coursera's "R Programming",
# https://www.coursera.org/course/rprog.
#
# The goal is to implement two functions that support a special matrix object
# that caches its inverse matrix. See the associated readme file for details.

# Creates the matrix object from specified initial matrix.
# The initial matrix must be invertible.
# Parameters: original - the initial matrix for which this function
# creates the matrix object.
# Returns: the matrix object that caches its inverse matrix.
makeCacheMatrix <- function(original = matrix()) {
    message("Creating matrix object")
    inverse <- NULL
    
    get <- function() original
    set <- function(x) {
        original <<- x
        inverse <<- NULL
    }
    getinverse <- function() inverse
    setinverse <- function(x) inverse <<- x
    
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}

# Returns the inverse matrix of the specified matrix object.
# Parameters: obj - matrix object created previously by
# the makeCacheMatrix function.
# Returns: the inverse matrix of the specified matrix object.
cacheSolve <- function(obj, ...) {
    inverse <- obj$getinverse()
    
    if (!is.null(inverse)) {
        message("Returning cached inverse matrix")
        return(inverse)
    }
    
    message("Computing inverse matrix")    
    original <- obj$get()
    inverse <- solve(original, ...)
    obj$setinverse(inverse)
    inverse
}
