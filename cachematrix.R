## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function

## This function gets a matrix as an input, set the value of the matrix, get the value of the matrix, 
## set the inverse Matrix and get the inverse Matrix. It allows the matrix object to cache its own object. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function 

## This function takes the output of the previous matrix makeCacheMatrix(matrix) as an 
## input and checks if the inverse matrix from it has any value in it or not.
## In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
## and set the invertible  matrix by using the solve function.

## In case inverse matrix from makeCacheMatrix((matrix) has some value in it, 
## returns a message "getting cached data for inverse matrix" and the cached object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data for inverse matrix")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}

## Testing Solution:

testMatrix <- matrix(1:4,2,2)
testMatrix
cacheMatrix <- makeCacheMatrix(testMatrix)
cacheSolve(cacheMatrix)
