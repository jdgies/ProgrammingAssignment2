## Programming Assignment #2 (week 3)
##
## "Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute
## it repeatedly (there are also alternatives to matrix inversion that
## we will not discuss here).
##
## Your assignment is to write a pair of functions that cache the inverse of a matrix."

## This function creates a matrix object and a list of functions that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        message("initializing cache functions")
        
        ## initialize inverse_matrix
        special_inverse <- NULL
        
        ## as in the example, this function creates a list of functions to:
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. set the value of the inverse_matrix
        ## 4. get the value of the inverse_matrix
        
        set <- function(y) {
                # assigning variable y to an object outside current environment
                x <<- y
                
                # matrix has changed so reset inverse
                special_inverse <<- NULL
        }
        
        get <- function() {
                # takes no arguments and simply returns the matrix 'x'
                x
        }
        
        set_inverse <- function(y) {
                special_inverse <<- y
        }
        
        get_inverse <- function() {
                return(special_inverse)
        }
        
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
        #return(x)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        # check if inverse is already calculated
        matrix_inv <- x$get_inverse()
        
        
        if (!is.null(matrix_inv)) {
                # if not NULL, return cached value
                message("getting cached data")
                return(matrix_inv)
        }
        # cache the inverted matrix for next time and also return it to calling routine
        x.data <- x$get()
        typeof(x.data)
        ## for this assignment, I get to assume that the matrix is invertible
        matrix_inv <- solve(x.data, ...)
        x$set_inverse(matrix_inv)
        
        return(matrix_inv)
        
}


# to test, create a normalized matrix (square)
m <- matrix(rnorm(1000000), nrow=1000, ncol=1000)
str(m)

# initialize inverse matrix variable and set up cache functions
fun_list <- makeCacheMatrix(m)

# invert matrix

cat("\n=== first run ===\n")
#let's see how long this takes
run1 <- system.time({
        test <- cacheSolve(fun_list)
})
print(run1)
str(test)


# run it again and see that the result is cached
cat("\n=== second run ===\n")
# how long does it take when it's cached?
run2 <- system.time({
        test<-cacheSolve(fun_list)
})
print(run2)
str(test)
