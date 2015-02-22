## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a list of four functions
## it takes as input a matrix

makeCacheMatrix <- function(x = matrix()) {
  # we set the inverse matrix to NULL. This is the case that will
  # be used when we call cacheSolve for the first time
  inv<-NULL
  get<-function() x # this returns the matrix x
  set<-function(y){ # we use this function if we want to change the values of the matrix
    x<<-y
    inv<<-NULL
  }
  getinv<-function() inv # this function returns the inverse matrix
  setinv<-function(inverse){ #this function sets the value of "inv" to the inverse matrix
    # we calculate in the cacheSolve function
    inv<<-inverse
  }
  list(get=get,set=set,getinv=getinv,setinv=setinv) #this is the output list. We assign names
  # to each of the functions
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  inv<-x$getinv() # first step is to get the value of "inv"
  if(!is.null(inv)){ # if it is already stored in the cache, we take it and return it
    message("getting cached data")
    return(inv)
  }
  # if it is not yet stored we have to calculate the inverse matrix
  data<-x$get() # first we get the matrix
  inv<-solve(data) # second we calculate the inverse and store it
  x$setinv(inv) # third we store inv in the cache through the setinv function
  inv
        ## Return a matrix that is the inverse of 'x'
}
