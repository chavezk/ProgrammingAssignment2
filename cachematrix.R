## The following functions with cache the inverse of a matrix
 
  
## Create a matrix that that contains the following functions
	##1. set the value of the vector
	##2. get the value of the vector
	##3. set the value of the mean
	##4. get the value of the mean

##The list generated will be the input to cacheSolve()



  
makeCacheMatrix <- function(x = matrix()) {
 
	inv = NULL
        set = function(y) {
 		x <<- y
                inv <<- NULL
 	}
         
	get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
} 
  
  
 ## Returns the inverse matrix from the makeCacheMatrix()
  
cacheSolve <- function(x, ...) {

        
        inv = x$getinv()
        
        
        if (!is.null(inv)){
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
