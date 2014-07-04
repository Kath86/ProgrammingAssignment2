## This function creats a special "matrix" object that can cache its inverse.
## 1. set the mean to NULL for starters
## 2. create a setter that caches x and m
## 3. create a getter that returns the cached x
## 4. create a getInverse function that returns the solve, only calculating it if necessary
## 5. return the makeCacheMatrix object as a list of 4 functions
                                                                   
 makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y){
                 x <<- y
                 m <<- NULL
         }
         get <- function(){
                 x
         } 
         ## defines a function to get the values in a matrix
         setInverse <- function(solve) m <<-solve
         getInverse <- function() m
         list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)      
 }
         
                                                                                                                                      
                                                                  
                                                                   
 ## This function computes the inverse of the special"matrix" returned by 
 ## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
 ## then the cachesolve should retrieve the inverse from the cache.
              
                                                                   
cacheSolve <- function(x, ...) {                             
       ## Return a matrix that is the inverse of 'x'  
        m <- x$getInverse()
        
        ## in the case the inverse has already been calculated and the matrix has not changed 
        ## then the cachesolve function retrieves the inverse from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if the inverse has not yet been calculated
        data <- x$get()
        m <- mean(data, ...)
        x$setInverse(m)
 }
