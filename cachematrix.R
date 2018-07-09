# makeCacheMatrix - keeps a martix and caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
        
        # keeps cached value or NULL if nothing is cached
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(val) {
                x <<- val
                # new value assigned to matrix, flush cache
                cache <<- NULL
        }
		
        # returns stored matrix
        getMatrix <- function() {
                x
        }

        # cache the given argument 
        cacheInverse <- function(iVal) {
                cache <<- iVal
        }

        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list of function names
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}

# Calculates the inverse of matrix returned by makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get cached value
        inverse <- y$getInverse()
        # if cached value is present return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # else get the matrix, caclulate the inverse and store it in cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return inverse of matrix
        inverse
}