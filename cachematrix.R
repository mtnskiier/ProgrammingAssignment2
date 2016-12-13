## Caching the inverse of a Matrix
## 
## Purpose - Use the lexical scoping within R to cache the matrix inverse operation.
##

makeCacheMatrix <- function(x = matrix()) {
        ## INPUTS 
        ##      x - Is a inverted matrix to cache for future reference
        
        ## OUTPUTS
        ##      A list of methods accessing the original (x) and inverse value (mi)
        
        mi <- NULL
        
        set <- function(y) {
                x <<- y
                mi <<- NULL
        }
        
        get <- function() x
        setim <- function(inv_mat) mi <<- inv_mat
        getim <- function() mi
        
        list(set = set, get = get,
             setim = setim,
             getim = getim)

}



cacheSolve <- function(x, ...) {
        ## INPUTS
        ## x - a vector contining a source matrix and it's inverse, and all methods 
        ##     (see makeCacheMatrix)
        ##
        ## OUTPUTS
        ## mi - is the inverse of the original matrix
  
        
        mi <- x$getim()
       
        if (!is.null(mi)) {
                message("getting cached data")
                return(as.matrix(mi))
        }
        
        data <- x$get()
        print(data)
        ## solve for the inverse
        mi <- solve(data, ...) 
        ## cache the results
        x$setim(mi)
        ## return the inverse matrix
        return(as.matrix(mi))                                             
}
