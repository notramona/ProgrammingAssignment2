## Put comments here that give an overall description of what your
## functions do
## these functions will cache the inverse of a matrix, compute the inverse of a 
## cached matrix and retrieve the inverse from the cache.

## Write a short comment describing this function
## the function will create a special matrix to:
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the cache of the matrix
## 4.  get the value of the cache of the matrix

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
                      x <<- y
                      m <<- NULL
              }
              get <- function() x
              setcachematrix <- function(solve) m <<- solve
              getcachematrix <- function() m
              list(set = set, get = get,
                  setcachematrix = setcachematrix,
                  getcachematrix = getcachematrix)

}



## Write a short comment describing this function
## Compute the inverse of the matrix returned by 
## this function. If the inverse has
## already been calculated (and the matrix has not changed), 
##then `cacheSolve` function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
          m<-x$getcachematrix()
          if(!is.null(m)){
              message("getting cached data")
              return(m)
          }
          matrix<-x$get()
          m<-solve(matrix, ...)
          x$setcachematrix(m)
          m
  
}

