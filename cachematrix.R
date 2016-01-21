## makeCacheMatrix and cacheSolve work together to store and retrieve data to solve
## a system of equations given by a square invertible matrix. makeCacheMatrix defines
## functions to solve the matrix, get and set the original matrix and the inverted one.
## It stores the solution outside of the current environment, using the "<<-" operator.
## cacheSolve computes the inverse of the matrix. If it was computed before, it is 
## retrieved from the cache.

## YOU CAN TEST THE FUNCTIONs USING THE FOLLOWING CODE:
## originalMatrix=rbind(c(9,13,5,2),c(1,11,7,6),c(3,7,4,1),c(6,0,7,10))
## testMatrix=solve(originalMatrix)
## matrixObject=makeCacheMatrix(originalMatrix)
## invertedMatrix=cacheSolve(matrixObject)
## * Run the last code two times to see that data is retrieved from cache
## * Compare testMatrix and invertedMatrix: They should be the same

## Function makeCacheMatrix takes a matrix as input argument. inversematrix is set
## to NULL set function sets x equal to its input argumet and inversematrix to NULL
## get function gets the value of the input matrix, setinverse sets the inverse matrix
## using the solve command, getinverse is a function to retrieve the inverted matrix
## in case it was not yet computed. 

makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    set <- function(y) {
        x <<- y
        inversematrix <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inversematrix <<- solve
    getinverse <- function() inversematrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes the above created object as input parameter. In the beginning,
## inversematrix is NULL, so the evaluation of the if-statement is "FALSE". Thus,
## data from the function above is retrieved and the inverse matrix is computed. Then
## the inverse matrix is set. The inversematrix is returned at the end.

## From the second run on, the if-statement is "TRUE" (because in the first run, the
## inverted matrix was saved in the cache), so the inverse matrix is retrieved from
## the cache.

cacheSolve <- function(x, ...) {
    inversematrix <- x$getinverse()
    if(!is.null(inversematrix)) {
        message("getting cached data")
        return(inversematrix)
    }
    data <- x$get()
    inversematrix <- solve(data,...)
    x$setinverse(inversematrix)
    inversematrix
}
