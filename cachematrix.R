##The purpose of writing the pair of functions is to cache the inverse of a matrix so 
##that we do not need to repeatedly compute the inversion of a matrix.

## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) 
                im <<- solve
        getInverseMatrix <- function() im
        list(set = set, get = get,
             setInverseMatrix =  setInverseMatrix,
             getInverseMatrix =  getInverseMatrix)
}


## use R function solve to compute the inverse of the matrix object returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        im <- x$getInverseMatrix()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setInverseMa(im)
        im
}
