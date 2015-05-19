## This pair of functions create an object that stores a numeric marix and caches its inverse. 

## The first function (main function) creates a matrix that can cache its inverse.
## It returns a list of 4 functions (set, get, setinverse, getinverse)

makeCacheMatrix <- function(x = matrix()) {
        i<-NULL                 ##i will store the inverse
        set<- function(y){      
                x<<-y           ##changes the matrix stored in the main function if called
                i<<-NULL
        }
        get<-function (){       ## returns the value of x, the matrix stored in the main function
                x
        }
        setinverse<-function(inverse){    ##stores the value of the inverse of x in i
                i<<-inverse
        }
        getinverse<<-function(){        ##returns the value of the inverse
                i
        }
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) ##These functions are now available for the calling function.
}


## The second function (calling function) computes and returns the inverse of the matrix x, created by makeCacheMatrix. 
## If the inverse has already been calculated, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
       i<-x$getinverse()   
       if (!is.null(i)){
               message("getting cached data")
               return (i)
       }
       data<-x$get()            
       i<-solve(data)           ##actual computation of the inverse
       x$setinverse(i)          ##calls the function to cache the inverse
       i
}
