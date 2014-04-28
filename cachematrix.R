
## I believe the goal here is to understand variable scoping,
## also to define a class-like structure that's cached in memory 
## the actual solving bit is left to the hacker's vivid imagination 



## Here's a nifty little function that aims to cache a matrix object, and have the cached
## value stored in a more global variable
## it almost feels like we're defining a tiny class!
makeCacheMatrix <- function(x = matrix()) {
  
  
  ## set the inverse variable to null
  ## note the scope of the inv variable; we'll use this specific variable later using the superassignment operator <<-
  inv <- NULL
  
  
  ## here's a set function that does an initialization, sets the 'matrix' object to a specified value, sets the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## fetch the matrix object
  get <- function(){ 
    return x
  }
  
  ## time to set the inverse variable - the whole point is to access the super variable in a more global scope
  setInverse <- function(inverse){
    inv <<- inverse
  }
  
  ## get the contents of the inverse variable 
  getInverse <- function(){
    return inv
  }
  
  ## return a 'list' that points to the getter setter functions
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



## Here lies a function that'll do the actual inverse solving, but takes the 'list' from the previous function as an input
cacheSolve <- function(x, ...) {
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message('Fetchig cached data. Neat!')
    return(inv)
  }
  
  ## get the 'matrix' object
  data <- x$get()
  
  ## solve for the inverse
  inv  <- solve(data,...)
  
  
  ## store/cache the inverse
  x$setInverse(inv)
  
  ## Return the inverse matrix
  inv
}
