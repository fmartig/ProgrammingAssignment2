## This pair of functions cache the inverse of a matrix 

## The first function creates a matrix that can cache its inverse.
## It returns a list of 4 functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL                 ##i will store the inverse
        set<- function(y){
                x<<-y
                i<<-NULL
        }
        get<-function (){
                x
        }
        setinverse<-function(inverse){
                i<<-inverse
        }
        getinverse<<-function(){
                i
        }
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function computes and returns the inverse of the matrix
##returned by makeCacheMatrix ('x'). If the inverse has already been 
##calculated cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
       i<-x$getinverse()
       if (!is.null(i)){
               message("getting cached data")
               return (i)
       }
       data<-x$get()
       i<-solve(data)
       x$setinverse(i)
       i
}
