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
      [,1] [,2]
[1,]    1    3
[2,]    2    4
my_matrix$getInverse()
NULL
cacheSolve(my_matrix)
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix$getInverse()
      [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
my_matrix$get()
      [,1] [,2]
[1,]    2    1
[2,]    2    4
my_matrix$getInverse()
NULL
cacheSolve(my_matrix)
         [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
my_matrix$getInverse()
         [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333



