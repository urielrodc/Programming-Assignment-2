## Coursera Tarea: Programming Assignment 2: Lexical Scoping
##Uriel Rodriguez
##Funcion makeCacheMatrix: hace matriz en cache para procesarla mas rapidamente

makeCacheMatrix <- function(x = matrix()) 	{
											inversa <- NULL
											set <-function(y) 	{
																x <<- y
																inversa <<- NULL
																}
											get <-function() x
											setInverse <- function(inverse) inversa <<- inverse
											getInverse <- function() inversa
											list(set = set,
												 get = get,
												 setInverse = setInverse,
												 getInverse = getInverse)
											}

##Funcion cacheSolve: hace inversa de la matriz sobre cache

cacheSolve <-function(x, ...) 	{
								inversa <-x$getInverse()
								if (!is.null(inversa)) {
														message("getting cached data")
														return(inversa)
														}
								mates <-x$get()
								inversa <- solve(mates, ...)
								x$setInverse(inversa)
								inversa
								}