## This function allows for creating a matrix
## to use it with another function cacheSolve that will invert
## the matrix / uses "<<-" for global variable (to do cache)

makeCacheMatrix <- function(x = matrix()) {
        
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        
        get <- function() x
        setinv <- function(solvMtx) minv <<- solvMtx
        getinv <- function() minv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}



## The function cacheSolve computes de Inverse of a matrix and
## tests if the matrix has already been inverted to use the
## cached data 

cacheSolve <- function(x, ...) {
  
  ## x$get stores the input matrix, solve(data) inverts it
  ## the if statement test "minv" variable if full matrix
  ## (remember "<<-" assgnmt from makeCacheMstrix)
  
        minv <- x$getinv()
        
        if(!is.null(minv)){
        message("getting cached data")
        return(minv)
        }
  
  data <- x$get()
  minv <- solve(data)
  x$setinv(minv)
  minv      
}
    


