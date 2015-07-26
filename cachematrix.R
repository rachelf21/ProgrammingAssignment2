
makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object that can cache its inverse.
        
        inverseMatrix<-NULL
        
        set<-function(y) {
                x<<-y
                inverseMatrix<<-NULL
        }
        
        get<-function() x
        setInverse <- function(solve) inverseMatrix<<-solve
        getInverse <- function() inverseMatrix
                
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
        
}

cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        ##If the inverse has already been calculated (and the matrix has not changed), then the 
        ##cachesolve should retrieve the inverse from the cache.

        inverseMatrix <- x$getInverse()
        
        if(!is.null(inverseMatrix)){
                message("getting cached data")
                return(inverseMatrix)
        }
        
        matrixData <- x$get()   
        inverseMatrix<-solve(matrixData)
        x$setInverse(inverseMatrix)
        
        inverseMatrix
}
