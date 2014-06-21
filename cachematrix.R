## This function creats a special "matrix" object that can cache its inverse.
## 1. set the mean to NULL for starters
## 2. create a setter that caches x and m
## 3. create a getter that returns the cached x
## 4. create a getmen function that returns the mean, only calculating it if necessary
## 5. return the makeCacheMatrix object as a lis of 4 functions
                                                                   
 makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y){
                 x <<- y
                 m <<- NULL
         }
         get <- function(){
                 x
         } 
         setmean <- function(mean) m <<-mean
         getMean <- function(){
                 m
         } 
         list(set = set, get = get,setmean = setmean,getmean = getmean)      
 }
         
                                                                                                                                      
                                                                  
                                                                   
 ## This function computes the inverse of the special"matrix" returned by 
 ## by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
 ## then the cachesolve should retrieve the inverse from the cache.
              
                                                                   
cacheSolve <- function(x, ...) {                             
       ## Return a matrix that is the inverse of 'x'  
        m <- x$getmean()
        if(is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
 }