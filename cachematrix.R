## cache store of the computation of the inverse of a matrix
## 

## Input of the matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        #invm = inverse matrix as a empty variable
        invm <- NULL
        #asiggn a value to setivm 
        setivm <- function(y) {
                x <<- y
                invm <<- NULL
        }
        
        getivm <- function() x
        #using solve to obtain the inverse
        setinvmatrix <- function(solve) invm <<- solve
        getinvmatrix <- function() invm
        list(setivm = setivm, getivm = getivm,
             setinvmatrix = setinvmatrix,
             getinvmatrix = getinvmatrix)
        
}


## Use makecachematrix to compute the inverse of a matrix or not if is stored in cache

cacheSolve <- function(x, ...) {
        #check if there is already data stored for invm
        invm <- x$getinvmatrix()
        if(!is.null(invm)) {
                message("getting cached data")
                return(invm)
        }
        #If not it calculate the inverse
        data <- x$getivm()
        invm <- solve(data, ...)
        
        ## set and return a matrix that is the inverse of 'x'
        x$setinvmatrix(invm)
        invm
}
