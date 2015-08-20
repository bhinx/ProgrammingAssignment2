## This is an exercise to work with scoping.

# The first function allows a user to set a matrix to a stored variable
# The returned object has a set of funtions (mostly accessors) for working
# with that matrix.
#
# The primary goal of this is to store the inverse of the user provided matrix.
#
#
# The second function provides the mechansim to retrieve the user provided matrix
# from the original function and then build the inverse and store it.


# This function allows storing a original matrix and then storing a inverse matrix.
# Accessors to help with this are provided.
#
makeCacheMatrix <- function(mat.val = matrix()) {

  # initialization, there is no inversed matrix and it will have to be built.
  #
  inverse.mat <- NULL
  inverse.built <- FALSE
  
  # set a new initial matrix
  #
  set <- function(new.mat.val)
  {
    mat.val <<- new.mat.val
    inverse.mat <<- NULL
    inverse.built <<- FALSE
  }
  
  # get the initial matrix
  #
  get <- function() mat.val
  
  # get the flag which indicates if the inverse matrix has already been built.
  #
  get.inverse.built.flag <- function() inverse.built
  
  # if the above flag indicates a build has alread occurred, return the stored
  # inverse
  #
  get.inverse <- function() inverse.mat
  
  # set the inverse that has been calculated outside the makeCacheMatrix object
  #
  set.inverse <- function( inverted.matrix )
  {
    inverse.mat <<- inverted.matrix
    inverse.built <<- TRUE
  }
  
  list( set = set, get = get, get.inverse.built.flag = get.inverse.built.flag,
        get.inverse = get.inverse, set.inverse = set.inverse )
}


# This function retrieves a matrix from the list contructed by makeCacheMatrix. The 
# inverse of the matrix is then built and stored back into the cache.
#
cacheSolve <- function(x, ...) {
  resp.mat <- NULL
  
  if( x$get.inverse.built.flag() == FALSE )
  {
    # we need to build the inverse
    # 1. grab the matrix
    # 2. run the solve
    # 3. store it
    #
    x$set.inverse( solve( x$get() ) )
    resp.mat <- x$get.inverse()
  }
  
  ## Return a matrix that is the inverse of 'x'
  resp.mat <- x$get.inverse()
}
