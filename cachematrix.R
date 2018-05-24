
#guarda la inversa de la matriz 
makeCacheMatrix <- function (x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <-function() m
  list(set = set, get= get , setinv = setinv , getinv = getinv)
}
#calcula la inversa de una matriz (sumamente descriptivo)
cacheinv <- function(x, ...)
{
  m <- x$getinv()
  if(!is.null(m))
  {
    #print(" calculated ")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  return(m)
}

