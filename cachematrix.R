# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than compute it repeatedly.
# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_mat <- NULL
        set <- function(y) {
                x <<- y
                inv_mat <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_mat <<- inverse
        getinverse <- function() inv_mat
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# The following function returns the inverse of the matrix. 
# First checks if the inverse has already been computed.
# If so, it gets the result from the cache skipping the calculus.
# If not, it computes the inverse and sets the value in the cache.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv_mat <- x$getinverse()
        if(!is.null(inv_mat)) {
                message("getting cached data.")
                return(inv_mat)
        }
        data <- x$get()
        inv_mat <- solve(data)
        x$setinverse(inv_mat)
        inv_mat
}

## Sample Run: http://www.vitutor.com/algebra/determinantes/inversa.html
## ---------------------------------------------------------------------
## creating the same matrix as vitutor.com
# > x = rbind(c(2, 0, 1), c(3, 0, 0), c(5, 1, 1))
# > m = makeCacheMatrix(x)
# > m$get()
# [,1] [,2] [,3]
# [1,]    2    0    1
# [2,]    3    0    0
# [3,]    5    1    1

## first execution => no caching
# > cacheSolve(m)
# [,1]       [,2] [,3]
# [1,]    0  0.3333333    0
# [2,]   -1 -1.0000000    1
# [3,]    1 -0.6666667    0

## second execution => getting cached data
# > cacheSolve(m)
# getting cached data.
# [,1]       [,2] [,3]
# [1,]    0  0.3333333    0
# [2,]   -1 -1.0000000    1
# [3,]    1 -0.6666667    0
