## R Programming - Assignment 2
## These functions allow the inverse of a matrix, a potentially time-consuming 
## computation, to be cached.
##
## Caching is implemented via R's superassignment operator (<<-)
## From R help:
## The operators <<- and ->> are normally only used in functions, and cause a 
## search to made through parent environments for an existing definition of 
## the variable being assigned. If such a variable is found (and its binding 
## is not locked) then its value is redefined, otherwise assignment takes place
## in the global environment.


## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse.
## The "matrix" is actually a list containing four functions:
##	set, which sets the matrix and nullifies any cached inverse
##	get, which gets the matrix
##	setInverse, which sets the inverse of the matrix in the cache
##	getInverse, which gets the inverse of the matrix from the cache
makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
	## The set function uses the <<- operator (superassignment) to assign 
        ## values to the variables x and xinv in the parent environment
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
	## The setInverse function uses the <<- operator to assign the 
        ## given value to the variable xinv in the parent environment
        setInverse <- function(y) xinv <<- y
        getInverse <- function() xinv
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve: This function returns the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
##
## If the inverse has already been calculated,
## then the the inverse is retrieved from the cache and returned.
##
## If the inverse hasn't been calculated,
## then the the inverse is calculated and returned.
##
## This function assumes the matrix to be inverted is square and invertible.
cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
	## If the inverse is found in the cache, simply return it
        if(!is.null(inv)) {
                message("Getting cached data ...")
                return(inv)
        }
	## Otherwise get the matrix, and calculate, cache and return its inverse
        m <- x$get()
        inv <- solve(m, ...)
        x$setInverse(inv)
        inv
}

## Testing
m = matrix(c(2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2, 0, 0, 0, 0, 2), nrow=4, ncol=4)
cm <- makeCacheMatrix(m)
cm$get()
cacheSolve(cm)
cacheSolve(cm)

cm$set(matrix(c(4, 0, 0, 0, 4, 0, 0, 0, 4), nrow=3, ncol=3))
cm$get()
cacheSolve(cm)
round(cm$get() %*% cacheSolve(cm), 4)

hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
m <- hilbert(6)
cm$set(m)
cm$get()
cacheSolve(cm)
round(cm$get() %*% cacheSolve(cm), 4)

