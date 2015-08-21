#> Program Submitted for the Coursera R-Programming Assignment - 2
#> Program originaly written by: rdpeng
#> Modified by: Jose F. de Leon 08/22/2015 rev. 1

#> This program computes and stores the inverse of a matrix for use in future computations.
#> It contains 2 main functions makeCacheMatrix() and CacheSolve().

#> makeCacheMatrix() - creates a matrix that would contain the original matrix and an equally dimensioned matrix that would contain the inverse of the original matrix.
#> cacheSolve() - solves the original matrix created in the makeCacheMatrix function. if the original matrix is the same as the previous matrix submitted (ie. the same one previously solved) the cached solution to the matrix is returned.

#> How to use:
#> 1. makeCacheMatrix(x)
#> 2. cacheSolve(x)

########################################################
########################################################

#> makeCacheMatrix() - creates two (2) matrices that would contain the original matrix (x) and the inverted matrix (m).

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


#> cacheSolve() - solves the original matrix (x) and stores the solution in the solution matrix (m). if the original matrix (x) contains a matrix previously solved then this function returns the value of (m). 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}
 #> Nothing Follows
