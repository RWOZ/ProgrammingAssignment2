makeCacheMatrix <- function(x = matrix()) {
    
# inv stores the cached inverse matrix
    
    inv <- NULL
    
# Setter for the matrix    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

# Getter for the matrix

    get <- function() x

# Setter for the inverse

    setinv <- function(inverse) inv <<- inverse

# Getter for the inverse
    getinv <- function() inv

# Return the matrix with our newly defined functions
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve Computes the inverse of the matrix. If the inverse is already
# calculated, it returns the cached inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()

# If the inverse is already calculated, message appears before returned
    if (!is.null(inv)) {
        message("Retrieving Cached Data")
        return(inv)
    }

#  Calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)

# Store the inverse
    x$setinv(inv)

# Returns value
    inv
}
