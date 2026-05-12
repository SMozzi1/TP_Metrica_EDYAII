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
-}

---- EJERCICIO 3 ----
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p t = insertarRecu p t 0 --insertarRecu nos ayuda a pasar el level actual
  where
    insertarRecu punto Empty level = 
        Node Empty punto Empty (level `mod` dimension punto)

    insertarRecu punto (Node izq raiz der eje) level
      | coord eje punto < coord eje raiz = 
          Node (insertarRecu punto izq (level + 1)) raiz der eje
      | otherwise = 
          Node izq raiz (insertarRecu punto der (level + 1)) eje



