## This function created an object that contains a matrix and a cached version of the matrix inverse
## f

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invCache <- NULL
  # set function to create subobject for matrix and inverse cache
  set <- function (y) {
    x <<- y
    invCache <<- NULL
  }

  # create function to access matrix portion of object
  get <- function() x

  # create function to access inverse cache portion of the object
  getcache <- function() invCache

  # create function to set inverse cache portion of the object
  setcache <- function(x) {
    invCache <<- x
  }

  # Function returns a list containing functions that can be accessed
  # The data is stored in the contents of the functions
   list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)

}


# This function takes an object created by the makeCacheMatrix function and checks for the existance of # a chached version of the inverse. If no chached vesion of the inverse exists the invers is generated # using solve and the object is updated with a chache of the inverse. In both cases the inverse of the # matrix is returned.

cacheSolve <- function(x, ...) {
  # Check the object to see if there is a cacheed version of the matrix that is already stored
  # If there is no cahed version stored compute the inverse and cache the inverse in the object
  # Check to see if there is slready a cache imbedded in the object
  # If there is already a chaced value returnit
  # Else compute the value and set the object cache

  cache <- x$getcache()
  if (is.null(cache)) {
    m <- x$get()
    result <- solve(m)
    x$setcache(result)
    message("setting cached data")
  } else {
    message("getting cached data")
    result <- x$getcache()
  }
  ## Return a matrix that is the inverse of 'x'
  result
}
