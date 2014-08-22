## cachematrix.R
## created on 8/20/2014
##
## A pair of functions that cache the inverse of a matrix
## and demonstrate the utility of caching results from complex
## operations.
## The functions are: makeCacheMatrix and cacheSolve
## 

## makeCacheMatrix is a function that ceates a special 
## "matrix" object that can cache its inverse.
## The input is a matrix and the output are four methods:
## set(), get(), setinverse(), getinverse()
## set() assigns a matrix
## get() returns the matrix
## setinverse() assigns the inverse of a matrix
## getinverse() returns the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) m <<- inverse
        
        getinverse <- function() m
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}




## cacheSolve is a function which computes the inverse of the 
## special "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse 
## from the cache.
## input is an object that contains a matrix
## output is the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        

}

 
## Example:
# assign a matrix
y <- makeCacheMatrix(matrix(1:4, 2, 2))

# return the matrix
y$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

# attempt to show the inverse of the matrix
y$getinverse()
# NULL
# no inverse matrix stored so return NULL

# calculate the inverse of the matrix
cacheSolve(y)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# attempt to show the inverse of the matrix
y$getinverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# this time it worked because it was already calculated

# calculate the inverse of the matrix a second time
cacheSolve(y)
# getting cached inverse
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# this time it was retrieved from cache, so no need to
# recalculate it.

# change the matrix using the set function
y$set(matrix(4:7, 2, 2))

# the matrix has changed
y$get()
# [,1] [,2]
# [1,]    4    6
# [2,]    5    7

# attempt to show the inverse of the matrix
y$getinverse()
# NULL
# since the matrix was changed, no inverse matrix is
# stored so return NULL

# calculate the inverse of the matrix
cacheSolve(y)
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2

# attempt to show the inverse of the matrix
y$getinverse()
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2
# this time it works because it was calculated

# calculate the inverse of the matrix a second time
cacheSolve(y)
# getting cached inverse
# [,1] [,2]
# [1,] -3.5    3
# [2,]  2.5   -2
# this time it was retrieved from cache, so no need to
# recalculate it. 