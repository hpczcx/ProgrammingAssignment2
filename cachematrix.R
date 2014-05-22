## The purpose of this programming is to write a pair of functions that cache the inverse of a matrix.
## Write a short comment describing this function
## The function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
               m<-NULL
               set<-function(y){                        ##set the value of the matrix
                      x<<-y
                      m<<-NULL
               } 
               get<-function() x                        ##get the value of the matrix     
               setinverse<-function(solve) m<<- solve   ##set the value of the inverse
               getinverse<-function() m                 ##get the value of the inverse
               list(set=set, get=get,
                    setinverse=setinverse,
                    getinverse=getinverse)
} 

## The function "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
          m<-x$getinverse()
          if(!is.null(m)){      ##check whether the inverse has already been calculated
          message("getting cached data")
          return(m)             ##if it has been calculated,get it from the cache 
          }                     ##and skips the computation
          data<-x$get()
          m<-solve(data, ...)   ##if not,it calculates the inverse of the matrix
          x$setinverse(m)
          m
}
