# Sometimes it is beneficial to cache a result rather than computing it
# each time.  Here is a pair of functions that caches the inverse of
# matrix!

# This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { # passing matrix into x.
     inv <<- NULL # inverse is null by default.
     set <- function(y) { # replace existing matrix.
          x <<- y
          inv <<- NULL
     }
     get <- function() x # get existing matrix.
     
     setinverse <- function(solve) inv <<- solve # set inverse matrix.
     getinverse <- function() inv # get existing inverse matrix.
              
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse
     )
}

# This function computes the inverse of the matrix from the above function.

cacheSolve <- function(x, ...) { 
     inv <- x$getinverse() #retrive by using getinverse.
     
     if(!is.null(inv)) { #check to see if the inverse matrix has been calculated.
          message("getting cached data") #if it is there, then return inv.
          return(inv)
     }
     
     data <- x$get() # otherwise, compute the inverse of matrix with solve.
     inv <- solve (data, ...) # saves the inverse of matrix in inv.
     x$setinverse (inv) # set x as this inv result.
     inv # Return a matrix that is the inverse of 'x'
}
