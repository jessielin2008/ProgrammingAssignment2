## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    i <-NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i<<-inv
    getinverse <- function() i
    list(set=set, get=get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    mtx <- x$get()
    if (det(mtx) != 0){
        i <- solve(mtx)
        message("calculate inverse matrix and set the value")
        x$setinverse(i)
        i
    }else{
        message("the matrix is not inversable")
    }
        
}
