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

  # don't understand that this is doing
   list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #check the oject to see if there is a cacheed version of the matrix that is already stored
  # if there is no cahed version stored compute the inverse and cache the inverse in the object

  #check to see if there is slready a cache imbedded in the object

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
  #if there is already a chaced value returnit
  #else compute the value and set the object cache

  result
}
