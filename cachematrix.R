## the two functions below retrieves an inverse of a matrix if it exists in memory.
##If not they find the inverse and caches it for later use.

## makeCacheMatrix sets and retrieves inverse of a matrix.
##It gets a list of functions and has access to them and objects (x and cacheMat).

makeCacheMatrix <- function(x = matrix()) {
        
        ## initiates cacheMat and sets to NULL
        cacheMat <- NULL

        setMat <- function(y) {
                
                ## y gets assigned to x in the makeCacheMatrix environment
                x <<- y
                ## cacheMat gets NULL
                cacheMat <<- NULL
        }
        
        getMat <- function() x
        
        ## setInv gets the inverse of the matrix and assigns to cacheMat that is in makeCacheMatrix environment
        setInv <- function(solve) cacheMat <<- solve
        
        ## if cacheMat exists then getInv gets it
        getInv <- function() cacheMat
        
        ## returns list of the above functions
        list(setMat = setMat,
             getMat = getMat,
             setInv = setInv,
             getInv = getInv)
}


## cahceSolve calls specific functions in the enviroment of makeCacheMatrix
## in order to find an inverse of a matrix

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        mat <- x$getInv()
        
        ## if getInv does NOT return a NULL mat, then the matrix inverse has been found in the cache
        if(!is.null(mat)) {
                message("getting cached data...")
                return(mat)
        }
        
        ## if mat is returned as NULL
        data <- x$getMat()
        
        mat <- solve(data, ...)
        
        x$setInv(mat)
        
        mat
}
