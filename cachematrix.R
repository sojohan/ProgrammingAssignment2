## Here is an overall description of my two functions:
## 1. makeCacheMatrix
## 2. cacheSolve
## Below I have written in more detail about these two functions. At the end
## I have added an example.

## Regarding 1. MakeCacheMatrix.
## Write a short comment describing this function
## The input for makeCacheMatrix is a matrix type and do the following:
## 1. set the value of the matrix
## 2. get the value of matrix
## 3. set value of the inverse of the matrix
## 4. get value of the inverse of matrix 
## in the program(MakeCacheMatrix) i have put 1-4(as a comment),where this is done

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL ## Set the mean to NULL (1) 

  ## The set function defines a new funtion that set the matrix x to a new matrix y
  ## and resets the inverse m to NULL (1).
  ## I have used `<<-` to assign a value to an object in an environment 
  ## different from the current environment. 
  set <- function(y) { 
    x <<- y
    m <<- NULL
  }
  ## The get function returns the matrix x (2)
  get <- function() x
  ## The setsolve function assign solve to m (3)
  setsolve <- function(solve) m <<- solve
  ## The getsolve returns the inverse value m (4)
  getsolve <- function() m
  ## The list command returns the vector of values of the functions just defined
  ## This list is an object that will be called from the cacheSolve function below
  list(set = set, get = get,
     setsolve = setsolve,
     getsolve = getsolve)

  }


## Regarding 2. cacheSolve.
## This function returns the inverse of (A^-1) if the matrix has not changed
## then it will return the cached value otherwise it re-calculates the inverse.
## 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve() ## assigns the value of x$getsolve to m
  if(!is.null(m)) { 
    ## !is.null(m) return FALSE if it is argument is null and TRUE otherwise
    ## if true it prints the messages and return the cached value m
    message("getting cached data")
    return(m)
  }
  ## In the case that the above if statement is FALSE:
  data <- x$get() ## get the value x$get() and assigns it to data
  m <- solve(data, ...) ## calculate the inverse matrix of data and assigns it to m
  x$setsolve(m) ## Sets the value of the inverse in cache with the setsolve function
  m ## Return the inverse matrix
       
}
## Try the following experiment: 
## v<-matrix(1:4,2,2)
## s<-makeCacheMatrix(v)
## cacheSolve(s)
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## cacheSolve(s)
## getting cached data
##        [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
