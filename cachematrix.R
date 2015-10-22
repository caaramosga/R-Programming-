## Este script presenta dos funciones que se utilizan para crear un objeto especial que almacena una matriz y
## su caché inverso.

## La primera función, makeCacheMatrix crea una "matriz" especial,
##, que es en realidad una lista que contiene una función de:
## 1) establecer el valor de la matriz (set_matrix)
## 2) obtener el valor de la matriz (get_matrix)
## 3) establecer el valor de la inversa (set_inverse)
## 4) obtener el valor de la inversa (get_inverse)


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## La siguiente función calcula la inversa de la "matriz" especial creado con la función anterior.
## Sin embargo, se comprueba primero para ver si la inversa ya ha sido calculado.
## Si es así, obtiene el inverso de la caché y se salta el cálculo.
## De lo contrario, se calcula el inverso de los datos y define el valor de la inversa en la memoria caché a través
## La función 'set_inverse'.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

##Probando las funciones

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
my_matrix$getInverse()
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
my_matrix$getInverse()
cacheSolve(my_matrix)
my_matrix$getInverse()

#This code was take from:
#http://xmuxiaomo.github.io/2015/06/14/R-Programming-Assignment-2/ 

