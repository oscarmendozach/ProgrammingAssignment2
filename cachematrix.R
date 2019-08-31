## These functions were written for the assignment 2, week 3 of the JHU DS Specialization
## the goal is to have a function (or functions) that can store the inverse in the cache
## to do faster computations

## The first function creates a Matrix 


makeCacheMatrix <- function(x = matrix()) {

  ## initialize the function, by giving it the value of NULL, we avoid 
  ## that inv would be filled with "trash" values
  inv <- NULL
  
  ## the matrix is set
  set <- function(y){
    x <<- y
    inv <- NULL
  }
  
  ## the matrix is returned
  get <-function() x
  
  ## set the inverse of the matrix
  setinverse <- function(inverse) inv<<-inverse
  
  ## get the inverse of the matrix
  getinverse <- function() inv
  
  ##list with all the methods
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
  
}


## Compute the inverse of the matrix returned by the function "makeCacheMatrix"
## If the inverse has been already conputed and the function remains the same, 
## the inverse should be retrieved from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Returns a matrix m which is an inverse of x
  inv <- x$getinverse()
  
  ##if the matrix exists,then it returns the inverse
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## returns the matrix
  data <- x$get()
  
  ## computes the inverse using the solve()  function
  inv <- solve(data,...)
  
  ## Set the inverse to the object
  x$setinverse(inv)
  
  ## Returns the inverse
  inv
}
