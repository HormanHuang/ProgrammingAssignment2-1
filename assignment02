#   ====================== makeCacheMatrix function =======================
#   which is I used for calculate the matrix and define get/set function
#   (1) set: assign a new matrix accoring to the args setting
#   (2) get: then can get this one by using get function
#   (3) setInverse: calculate the matrix within the scope
#   (4) getInverse: return the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL
      set <- function(y) {
            x <<- y
            mx <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse) mx <<- inverse
      getInverse <- function() mx
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}

#   ===========================cacheSolve function==================
#   store the inverted one into the mx variable
#   in the if statement, if it is not a null value, that means it has been calculated 
#   so that there is ni need to recalculate and just return the inverted one
#   then if it is not been calculated, just use the solve() to get data to x and return mx
cacheSolve <- function(x, ...) {
      mx <- x$getInverse()
      if(!is.null(mx)) {
            message("getting cached data")
            return(mx)
      }
      data <- x$get()
      mx <- solve(data, ...)
      x$setInverse(mx)
      mx
}

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

my_matrix$set(matrix(c(2, 2, 1, 4,9,5,6,5,3), 3, 3))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$getInverse()

