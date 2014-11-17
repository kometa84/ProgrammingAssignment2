## These two functions allow to create a matrix first and then to calculate its inverse
## as many times we want

## "makeCacheMatrix" allows to create a matrix and to set all the cache values that the function
##"cacheSolve" will use to calculate the inverse of the matrix created

makeCacheMatrix <- function(x = matrix()) {  # set that the x will be a matrix
  im <- NULL                             # im will be the inverse matrix and it's reset to NULL every 
                                             #time makeCacheMatrix is called
  set <- function(y) {                    #take the matrix that we create
    x <<- y                               #save the matrix created
    im <<- NULL                           #reset im (inverse matrix) to null, when a new matrix is created
  }
  get <- function() x                     #this function returns the value of the original matrix
  setim <- function(solve) im <<- solve   #this is called by cacheSolve () during the first cachesolve()
                                          #access and it will store the value using superassignment
  getim <- function() im                  # this will return the cached value to cachesolve() on
                                          #  subsequent accesses
  list(set = set, get = get,              #this is accessed each time makeCacheMatrixr() is called,       
       setmean = setmean,                #   that is, each time we make a new object.  This is a list of 
       getmean = getmean)  #   the internal functions ('methods') so a calling function
                            #   knows how to access those methods.
       setim = setim,
       getim = getim)
}


## "CacheSolve" takes the data from MakeCacheMatrix and uses them to calculate the Inverse of the
##matrix created

cacheSolve <- function(x, ...) {  #take the value of the original matrix that we set in MakeCachematrix
  im <- x$getim()                 #access to the matrix x and calculate its inverse
  if(!is.null(im)) {              #if mean was already cached (not NULL) it will send the following 
    message("getting cached data")#"getting cached data" 
    return(im)                    #it will return the inverse matrix already calculated
  }
  data <- x$get()                  # this code is reached only if x$getim() returned NULL
  im <- solve(data, ...)          # if im was NULL then we have to calculate the im
  x$setim(im)                     # store the calculated mean value in x (see setmean() in makeCahcheMatrix
  im                               # return the mean to the code that called this function
}


