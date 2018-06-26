## programs that will compute inverse of matrix, cache this, 
## and will retrieve cache value if already computed upon new runs

## makes list of run functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {                        #   this 
                       x <<- y                  #   part
                       m <<- NULL               #   defines
    }                                           #  
                                                #   the
    get <- function() x                         #  
    setSolve <- function(solve) m <<- solve     #   run
    getSolve <- function() m                    #   functions
    
    list(set = set, get = get,                  #   create list 
         setSolve = setSolve,                   #   of run
         getSolve = getSolve)                   #   functions
    }

## gets and returns cache if computed else does new run

cacheSolve <- function(x, ...) {      #  if
    m <- x$getSolve()                 #  already ran
    if(!is.null(m)) {                 #  message
      message("getting cached data")  #  getting cached data
      return(m)                       #  return cache
    }
  
    data <- x$get()                   #  if no run
    m <- solve(data, ...)             #  solve inverse 
    x$setSolve(m)                     #  of matrix
    m                                 #  output that
}

## mtx <- matrix(rexp(100), 10)    ##      example          ##        for 
## mtx2 <- makeCacheMatrix(mtx)    ##      of how to        ##      future
#  cacheSolve(mtx2)                ##         use           ##     reference
