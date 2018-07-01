## R Programming Assignment #2.  Setting and retrieving an inverse matrix from Cache. 
## Two Functions:  makeCacheMatrix and cacheSolvefunctions do

## makeCacheMatrix is the 1st called function to set the matrix in default values
## function does not perform the inverse but sets values used by cacheSolve

makeCacheMatrix <- function(x = matrix()) 
{
  set <- function(y)
  {
    minv <<- NULL
    x <<- y
  }
  
  get <- function() x
  
  setinv <- function(inv)
  {
    minv <<- inv
  }

  getinv <- function() minv
  
  list(get = get, set = set, 
       getinv = getinv, setinv = setinv)
}


## cacheSolve takes a matrix that was previously called in makeCacheMatrix
## if a previous call to cacheSolve placed an inverse matrix in cache it will retrieve it
## if the inverse matrix is not stored it will calculate the inverse and call
## makeCacheMatrix to store the values

cacheSolve <- function(x, ...) 
{
## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  if(!is.null(minv))
  {
    message("Getting Cached Matrix Inverse")
    return(minv)
  }
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv
}
