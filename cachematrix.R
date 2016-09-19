##
##  Two function    CREATE , STORE, AND RECALL  a matrix and its inverse
##  makeCacheMatrix creates matrix capable of running four functions 
## set stores the matrix in cache, get recalls the matrix 
## setInverse / getInverse it´s the same but for the inverse of the original matrix 
##


makeCacheMatrix <- function(x = matrix()) 
{     
  m <- NULL 
  set <- function(y)
  { 
    x <<- y   
    m <<- NULL #store matrix in cache  
  } 
  get <- function() x #get matrix 
  setInverse <- function(solve) m<<- solve #set inverse matrix 
  getInverse <- function() m #get inverse matrix 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)  ## create list of functions 
} 

##
## cacheSolve take a custom matrix type created by the makeCacheMatrix
## calculates the inverse matrix of it , first it checks to see if the calculation has been done before 
## if it then recalls the data from the cache. If it has not been done  calculates the inverse matrix then store it in the cache 
##

cacheSolve <- function(x, ...) 
{  
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()                 #query the x matrix's cache 
    if(!is.null(m))
    {              #the inverse has been previously calculated 
      message("getting cached data")    # sent message for cache  
      return(m)                         # return cache   
    } 
    data <- x$get()                     # get matrix used by makeCacheMatrix  
    m <- solve(data, ...)               # calculate inverse matrix 
    x$setInverse(m)                     # store the inverse matrix 
}
