## My goal is to obtain the inverse of the matrix. If the matrix is stored in 
## Cache, I'll want to retrieve that value else I'll compute the matrix inverse
## and store it in the Cache. I have written two functions to achieve the above
## goal

## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to 
## (a) Set the value of the matrix (b) Get the value of the matrix 
## (c) Set the inverse of the matrix (d) Get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmat <- function(y) {
                x<<-y
                m<<-NULL
        }
        getmat <- function() x
        setmatinv <- function(inverse) m <<- inverse
        getmatinv <- function() m
        
        list(setmat = setmat, getmat = getmat, 
             setmatinv = setmatinv, getmatinv = getmatinv)

}


## The following function calculates the inverse of the special "matrix" created 
## with the above function. However, it first checks to see if the inverse
## has already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. If not, it solves for the inverse of the matrix
## and sets the inverse of the matrix in the cache via the setmat function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmatinv()
        if(!is.null(m)){
                message("getting cached matrix")
                return(m)
        }
     else {
             data <- x$getmat()
             m <- solve(data)
             x$setmatinv(m)    
     }   
        m
}
