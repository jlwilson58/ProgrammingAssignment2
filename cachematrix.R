## Put comments here that give an overall description of what your
## functions do

## A pair of functions that cache the inverse of a matrix 


## Creates a special matrix object that can cache its inverse 

makeCacheMatrix <- function( m = matrix() ) { 
        
        i <- NULL   ## Initialize the inverse property
        
        set <- function( matrix ) { 
                m <<- matrix ()
                i <<- NULL 
        }
        
        get <- function() {m} 
        
        setinverse <- function(inverse) { 
                i <<- solve(m) 
        } 
        
        
        getinverse <- function() { 
                ## Return the inverse property 
                i 
        } 
        
        
        ## Return a list of the methods 
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse) 
} 




## Compute the inverse of the special matrix returned by "makeCacheMatrix" 
## above. If the inverse has already been calculated (and the matrix has not 
## changed), then the "cachesolve" should retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) { 
        
        ## Return a matrix that is the inverse of 'x' 
        m <- x$getinverse() 
        
        ## Just return the inverse if its already set 
        if( !is.null(m) ) { 
                message("getting cached data") 
                return(m) 
        } 
        
        ## Get the matrix from our object 
        data <- x$get() 
        
        
        ## Calculate the inverse using matrix multiplication 
        #  m <- solve(data) %*% data 
        m <- solve(data) 
        
        ## Set the inverse to the object 
        x$setinverse(m) 
        
        
        ## Return the matrix 
        m 
} 