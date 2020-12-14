## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  This function will create a list which has four elements:
## 1 - setdata -> sets the value of the input matrix
## 2 - getdata -> gets the value of the input matrix
## 3 - setsolve -> sets the action of inverting the matrix through solve()
## 4 - getsolve -> gets the action of inverting the matrix through solve()


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setdata <- function(y){
    x <<- y
    m <<- NULL
  }
  getdata <- function() x
  
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  
  list(setdata = setdata, getdata = getdata,
       setsolve = setsolve,
       getsolve = getsolve)
 }


## The following function takes the list "x" constructed with the previous
## "makeCacheMatrix(x = matrix())" function, and inverts the initial matrix of
## interest, returning (printing on console) its inverted result.
## If the matrix is being inverted for its first time, the function will not 
## return any additional message. If it is already stored, instead, it prints
## a message that is getting stored data, and it returns it directly through the 
## "if" statement, without having to re-execute the "solve" function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$getdata()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

