
makeCacheMatrix <- function(x = matrix()) {
        
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
        ## Return a matrix that is the inverse of 'x'

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
