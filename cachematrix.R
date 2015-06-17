## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        ## initialize m to null
        m<-NULL
        
        # set value
        set<-function(y) {
                ## assign y to x
                x<<-y
                m<<-NULL
        }
        
        ## retrieve matrix value
        get<-function() x
        
        ## assign value to "setsolve"
        setsolve<-function(solve) m<<-solve
        
        ## retrieve "solve" value
        getsolve<-function() m
        
        # the function has 4 internal sub functions: set, get, setsolve, and getsolve
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## Write a short comment describing this function
## computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## retrieve getsolve value
        m<-x$getsolve()
        
        ## if the value is not null, means the value has been cached
        if(!is.null(m)) {
                message("getting cached data")
        } else {
                ## get the raw data - initial matrix
                data<-x$get()
                ## calculate the inverse matrix by calling solve()
                m<-x$setsolve(solve(data))
                }   
        ## return m
        m
}

