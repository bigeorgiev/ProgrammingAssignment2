## Put comments here that give an overall description of what your
## functions do

## the functions is a list of 4 functions

makeCacheMatrix <- function(inputmatrix = matrix()) {
  ## inputmatrix: is a square invertible matrix
  ## return: the function returns a list containing functions to
  ##              > set =  set the matrix
  ##              > get = get the matrix
  ##              > setinv = set the inverse
  ##              > getinv = get the inverese
  ##         the result of the above is ised as an input into the cacheSolve() function
  
  inv = NULL
  set = function(y) {
    # `<<-` is used ot assigen a value to an object that is not in the current environement
    inputmatrix <<- y
    inv <<- NULL
  }
  get = function() inputmatrix
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## the function makes a decision whether the inverese of the 'variable' matrix has already been calculated

cacheSolve <- function(inputmatrix,c..) {
  ## inputmatrix is the output of makeCacheMatrix()
  ## return: return the innverse of the original matrix input to makeCacheMatrix()
  
  inv = inputmatrix$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv)
  }
  
  # else calculate the inverse
  mat.data = inputmatrix$get()
  inv = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache 
  inputmatrix$setinv(inv)
  
  return(inv)
  ## Return a matrix that is the inverse of 'inputmatrix'
  }