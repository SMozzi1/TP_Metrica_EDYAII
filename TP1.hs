import Data.List --Para el sortBy del ej 2

data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
              deriving (Eq, Ord, Show) 


class Punto p where
    dimension :: p -> Int -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada kesima de un punto (comenzando de 0)
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos

-- 1A, GENERICA, USANDO coord Y dimension
dist2 :: Punto p => p -> p -> Double
dist2 p q | dimension p /= dimension q = 0
         | otherwise = sqrt (sum [(coord i q- coord i p)^2 | i <- [0.. dimension p - 1]])


--1B
newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)
    
distancia2d :: Punto2d -> Punto2d -> Double
distancia2d (P2d (x1,x2))  (P2d (y1,y2)) = sqrt((y1 - x1)^2 + (y2 - x2)^2)

distancia3d :: Punto3d -> Punto3d -> Double
distancia3d (P3d(x1,x2,x3)) (P3d (y1,y2,y3)) = sqrt((y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2)

-- Instancias (lo hizo Mozzi)

instance Punto p_2d where

    dimension _ = 2

    coord k (P2d (x, y))
    | k == 0 = x
    | k == 1 = y
    | otherwise = error "Dimension fuera de rango"
    
    dist p q = distancia2d p q 

instance Punto p_3d where
    dimension _ = 3

    coord k (P3d (x, y, z))
    | k == 0 = x
    | k == 1 = y
    | k == 2 = z
    | otherwise = error "Dimension fuera de rango"

    dist p q = distancia3d p q

{-

POR QUE INSTANCIAMOS WACHINES nada al final si las funciones son con puntos genericos pero si queremos
correr el programa y que compile necesitamos algun que otro caso instanciado para poder ejecutarlo
sobre un tipo de dato bien definido

-}

---- EJERCICIO 2 ----

--Funcion generica, que toma un indice y dos puntos, y compara sus valores en ese indice
compareidx :: Punto p => Int -> p -> p -> Ordering
compareidx i p q = compare (coord i p) (coord i q)

-- sortBy (compareidx i) listadepuntos
-- esto nos va a servir para calcular la mediana
-- y elegir el nodo padre

fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList puntos = aux puntos 0 --Defino aux, que nos ayuda a pasar el level actual (arranca en 0)
    where
        aux [] _ = Empty 
        aux pts level =
            let
                d = dimension (head pts) --La dimension de un punto es la misma que el resto
                eje = level `mod` d 

                --ORDENO PUNTOS
                ptsordenados = sortBy (compareidx eje) pts --Ordeno pts segun compareidx en el eje actual
                len = length ptsordenados
                mid = len `div` 2 -- La mediana es el valor medio de una lista ordenada

                --DIVIDO EN l Y R
                izq = take mid ptsordenados --toma mid puntos
                der = drop (mid+1) ptsordenados --dropea mid + 1 puntos, no me quedo la mediana
                raiz = ptsordenados !! mid
            in Node (aux izq (level + 1)) raiz (aux der (level + 1)) eje

{-PROBLEMAS DE fromList
Complejidad O(nlog^2n), no es mala dentro de todo

Mozzi: Creo que no se puede zafar del costo. no veo la forma de no ordenar cada recursion
-}

---- EJERCICIO 3 ----
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p t = insertarRecu p t 0 --insertarRecu nos ayuda a pasar el level actual
  where
    insertarRecu punto Empty level = 
        Node Empty punto Empty (level `mod` dimension punto)

    insertarRecu punto (Node izq raiz der eje) level
      | coord eje punto < coord eje raiz = 
          insertarRecu punto izq (level + 1)
      | otherwise = 
          insertarRecu punto der (level + 1)


{- No funciona porque en cada paso crea el nodo. lo que hay que hacer para mantener el balance es en cada paso
comparar las coordenadas del nivel en el que estas parado para determinar si vas a la izquierda o a la derecha.
si estas en el nivel 0 comparas los valores del eje x si estas en el nivel 2 los del eje y y asi. insertas en las hojas-}


-- Ejercicio 4 (Mozzi) --

eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p

{-

Lo que pensé: los niveles de mi arbol van a ir indicando el eje sobre el que estoy cortando y sabeos que el eje se determina como
nivel del arbol en el que estoy % dimension del punto. entonces la busqueda del punto a eliminar lo hago como en un arbol de busqueda
común. cuando encuentro el nodo si es una hoja lo elimino y listo. sino, al igual que como haciamos con un BST común, tengo que 
buscar el nodo mas chico del subarbol derecho o el nodo mas grande del subarbol izquierdo para mantener el invariante de un arbol
binario de busqueda. la diferencia principal es que la busqueda de este nodo maximo o minimo va a terminar no cuando sea una hoja sino
cuando el eje coincida con el eje del nodo por sobre el cual estoy eliminando. para esto vamos a usar dos funciones auxiliares masGrande y masChico

Tenemos que considerar dos cosas. la primera es siempre que buscamos comparar sobre el eje que estamos buscando y la segunda es que el nodo retornado
tiene que ser el ultimo nodo de ese eje. por lo tanto de alguna manera tenemos que llevar registro de "el mejor candidato"

Para esto lo que podemos hacer es crear un valor mejor candidato y que su valor sea una llamada recursiva que pare cuando llegue a una hoja
pero que se reemplace solamente si el eje de la hoja es el mismoo que el eje sobre el que se está buscando

La idea tiene errores y contraejemplos. es por acá pero todavia no cierra

Lo que pensamos con el pola: El eliminar elimina segun el valor de cada nodo en el eje que se desea eliminar. luego se llama al eliminar
recursivamente en el subarbol de donde se eligio pero ahora llamando al nodo que se inserto en la raiz (porqu eestaria duplicado) y esta recursion
se va a hacer hasta llegar a la hoja

el costo es h * log(n) con h la altura del arbol

-}

masGrande :: (Eq p, Punto p) => NdTree p -> NdTree p -> NdTree p

masGrande _ Empty = Empty
masGrande p (Node l raiz r eje) = 



