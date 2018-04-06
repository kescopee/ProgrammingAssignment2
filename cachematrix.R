## This file contains 2 functions that create a special matrix object and cache them to memory.
## This is suitable for performing repeated task, instead of creating and retrieving variables 
## every run, functions cache variables/matrix which make computation run faster.


## The 'makeCacheMatrix' function creates a  matrix object that can cache
## its inverse. This function also returns a list of fucntions to retrieve
## or set the matrix created as well as to set and retrieve the cached matrix.
makeCacheMatrix <- function(x = matrix()) {
        # the invmat is initially assigned a NULL value
        invmat <- NULL
        # the set function sets the matrix to be cached as x
        # and I_invmat as NULL so we can know when cachSolve runs (atleast once)
        set <- function(y){
                x <<- y 
                invmat <<- NULL
        }
        # similar to getters and setter in other OOP languages
        # the 'get' function returns the value of x (i.e. matrix)
        get <- function() x 
        # sets the value of l_invmat in cache to the value of invmat (basically a setter function)
        setinverse <- function(l_invmat) invmat <<- l_invmat
        # this function retrieves the inverted matrix
        getinverse <- function() invmat
        # Below is list the holds all functions created within the 'makeCacheMatrix'
        # which can be used to perform specific function calls e.g set and get the matrix
        list(
                set=set,
                get=get,
                setinverse=setinverse,
                getinverse=getinverse
        )
}

# The above function can be used as follows::
# 
# > mat <- makeCacheMatrix()
# > mat$set(matrix(c(1,2,3,4),2,2))
# > mat$get()


# The 'cacheSolve' function returns the invers of a matrix.  The functions firsts 
# checks if the inverse has been computed and return its value. If no, it computes 
# the inverse of the matrix and set the value in cache using the 'setinverse' function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # get the current state of the matrix, 
        # to see if the inverse has been computed
        invmat <- x$getinverse()
        # if true then return the inverse matrix
        if(!is.null(invmat)){
                message("Retrieving cached data")
                return(invmat)
        }
        # if not retrieve the matrix it self
        data <- x$get()
        # invert the matrix using the solve function
        invmat <- solve(data, ...)
        # Cache the inverted matrix
        x$setinverse(invmat)
        # return the result
        invmat
}


# The above function can be used as follows :
#
# On first run  no cache
# > cacheSolve(mat) 
#
# Cached data will be returned on second run
# > cacheSolve(mat)