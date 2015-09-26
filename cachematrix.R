##At the end of this file I have written a short guide to test both functions

## This is a 'constructor' function. This function makes an object...
##... of the type makeCacheMatrix to hold a matrix and...
##... two hulpfunctions to operate on them: 'get' and 'set'...
##... and an inverse of this matrix and ...
##...two function to operate on them: 'getinv' and 'setinv'
## However, this function doesn't compute the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the matrix.
## The function needs an object of the type makeCacheMatrix as an input.
## The function tests first if the inverse of the matrix exists by using 'getinv'.
## If the inverse exists then a message is thrown and the existing inverse is returned.
## If the inverse doesn't exist then it is computed and added to object by 'setinv'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
    }

##To test this function we can use the fact that the matrix multiplication...
##... of a matrix and his inverse gives the identity matrix.
##To do this we have to:

#(1) declare a clean matrix: 
#myM<-matrix(1:4,ncol=2,nrow=2)

#(2) declare an object of the type makeCacheMatrix (myCacheM) based on myM
#myCacheM<-makeCacheMatrix(myM)

#(-) by the way - check that you can get a matrix by:
#myCacheM$get()
#however the inverse is not yet computed (NULL), check:
#myCacheM$getinv()

#(3) compute the inverse
#cacheSolve(myCacheM)

#(-) by the way - check that the inverse is now computed and cached
#myCacheM$getinv()

#(4) make the matrix multiplication (use %*%):
#myCacheM$get() %*% myCacheM$getinv()

#(-) the results have to be the identity matrix:
#       [,1] [,2]
#[1,]    1    0
#[2,]    0    1



