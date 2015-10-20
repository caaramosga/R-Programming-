## Este script presenta dos funciones que se utilizan para crear un objeto especial que almacena una matriz y
## su caché inverso.

## La primera función, makeCacheMatrix crea una "matriz" especial,
##, que es en realidad una lista que contiene una función de:
## 1) establecer el valor de la matriz (set_matrix)
## 2) obtener el valor de la matriz (get_matrix)
## 3) establecer el valor de la inversa (set_inverse)
## 4) obtener el valor de la inversa (get_inverse)


makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL 					## Inicialmente se asigna 'NULL' para invertida
  set_matrix <- function(y) {			
    x <<- y 					## Ajustando la matriz 'x'
    inverse <<- NULL
  }
  get_matrix <- function() x 				## Retorna matriz 'x'
  set_inverse <- function(solve) inverse <<- solve 	## Cache the value of the inverse 
  get_inverse <- function() inverse 			## Retorna invertida
  list(set_matrix = set_matrix, get_matrix = get_matrix,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

## La siguiente función calcula la inversa de la "matriz" especial creado con la función anterior.
## Sin embargo, se comprueba primero para ver si la inversa ya ha sido calculado.
## Si es así, obtiene el inverso de la caché y se salta el cálculo.
## De lo contrario, se calcula el inverso de los datos y define el valor de la inversa en la memoria caché a través
## La función 'set_inverse'.

cacheSolve <- function(x, ...) {				## retorna una matriz que es la inversa de'x'
  inverse <- x$get_inverse()				## obteniendo la inversa
  if(!is.null(inverse)) {					## comprobando la presencia de la inversa
    message("getting cached data")			## mostrando el mensaje
    return(inverse)
  }
  data <- x$get_matrix()					## obteniendo la matriz
  inverse <- solve(data, ...)				## usando solución() para calcular la inversa
  x$set_inverse(inverse)					## para el cache inversa
  inverse 						## Regresa la inversa
}



