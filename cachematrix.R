## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than computing it repeatedly.

## This pair of functions caches the inverse of a matrix.


## This function creates a special "matrix" object
## that can cache its inverse.
# (NOTE: I deviated from the pattern outlined in the assignment
# towards a more (IMO) object-oriented implementation/factory pattern.
# makeCacheMatrix returns a functor with only one purpose in life:
# to access a (possibly cached) result. No getter or setter for the matrix
# beyond the original object construction, and no setter for the result:
# only this object alone owns and controls it.)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # this is our cached inverted matrix
  fsolve <- function() {  # "getter" function is really all we need
    if (is.null(inv)) {
      message("Solving for the 1st time...")
      inv <<- solve(x)  # actually inverting x here -- only once!
    } else {
      message("Returning cached solution...")
    }
    return(inv)
  }
  return(list(solve = fsolve))  # expose the above getter as "$solve"
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cacheSolve retrieves the inverse from the cache.
# ("Deviant" NOTE: this function is utterly unnecessary in this
# implementation and is only provided to maintain compatible
# "interface". A simpler call sequence is just:
# cx <- makeCacheMatrix(x)
# cx$solve()  # pay the price...
# cx$solve()  # ...reap the benefits
# )

cacheSolve <- function(x, ...) {
  return(x$solve())
}
