## Here is an overall description of my two functions:
## 1. makeCacheMatrix
## 2. cacheSolve
## Below I have written in more detail about these two functions. At the end
## I have added a couple examples.

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
  ## and resets the inverse m to NULL (1)

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
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
       
}
