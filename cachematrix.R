## Using the scoping operator to cache the inverse of a matrix

#the below function creates a object mat that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## function to set the matrix
    set <- function(matrix)
    {
        x <<- matrix
        inv <<- NULL
    }
    
    ## function to get the matrix
    get <- function()
    {
        x
    }
    
    ## function to set the inverse of the matrix
    setInverseofMatrix <- function(inverseObject)
    {
        inv <<- inverseObject
    }
    
    ## function to get the inverse of the matrix
    getInverseofMatrix <- function()
    {
        inv
    }
    
    list(set = set, get = get, setInverseofMatrix = setInverseofMatrix, getInverseofMatrix = getInverseofMatrix)
    
}


## "cacheSolve" computes the inverse of the matrix returned by "makeCacheMatrix"
## If the inverse has already been computed then "cacheSolve" should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
    
        ## return a matrix that is the inverse of "x"
        mat <- x$getInverseofMatrix()
        
        if(!is.null(mat))
        {
            message("getting cached data")
            return(mat)
        }
        
        result <- x$get()
        ## calculating the inverse using solve() method
        mat <- solve(result) 
        x$setInverseofMatrix(mat)
        ## return the matrix
        mat
}
