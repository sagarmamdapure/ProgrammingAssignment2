##The first function, makeCacheMatrix creates a special "vector", 
##which is really a list containing a function to
##1.set_mat                 set the value of the vector
##2.get_mat                 get the value of the vector
##3.set_Imat_mean           set the value of the mean
##4.get_Imat_mean           get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        matx <- NULL
        set_mat <- function(i) {
                x <<- i
                matx <<- NULL
        }
        get_mat <- function() x
        set_Imat_mean <- function(Imatx) matx <<- Imatx
        get_Imat_mean <- function() matx
        list(set_mat = set_mat, get_mat = get_mat,set_Imat_mean = set_Imat_mean,get_Imat_mean = get_Imat_mean)
        
        
}



##This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## @param: x must be cached and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Imatx <- x$get_Imat_mean()
        if(!is.null(Imatx)) {
                message("getting cached data")
                return(Imatx)
        }
        data <- x$get_mat()
        Imatx <- solve(data)
        x$set_Imat_mean(Imatx)
        Imatx
}

