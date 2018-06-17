## Functions makeCacheMatrix and cacheSolve are defined in this source file

## Function makeCacheMatrix initializes the matrix passed as argument x
## It defines the matrix x and its inverse inv in the parent env

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setinverse <- function(solve) inv <<- solve
        
        getinverse <- function() inv
        
        list(
                set = set,
                get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Function cacheSolve uses the matrix initialized by function
## makeCacheMatrix and checks if its inverse has already been calculated
## If so it uses the value from the variable in the parent environment
## Else it computes the inverse using the solve function and then sets the
## cache in the parent environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
        
        
}
