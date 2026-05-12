data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
              deriving (Eq, Ord, Show) 


class Punto p where
    dimension :: p -> Int -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada kesima de un punto (comenzando de 0)
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos

-- 1A, GENERICA, USANDO coord Y dimension
dist2 :: Punto p => p -> p -> Double
dist2 p q | dimension p \= dimension q = 0
         | otherwise = sqrt (sum [(coord i q- coord i p)^2 | i <- [0.. dimension p - 1]])


--1B
newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)

distancia2d :: Punto2d -> Punto2d -> Double
distancia2d (P2d (x1,x2))  (P2d (y1,y2)) = sqrt((y1 - x1)^2 + (y2 - x2)^2)

distancia3d :: Punto3d -> Punto3d -> Punto3d -> Double
distancia3d (P3d(x1,x2,x3)) (P3d (y1,y2,y3)) = sqrt((y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2)

-- EJERCICIO 2

compareidx :: Punto p => Int -> p -> p -> Ordering
compareidx i p q = compare (coord i p) (coord i q)

-- sortBy (compareidx i) listadepuntos
-- esto nos va a servir para calcular la mediana
-- y elegir el nodo padre

fromList :: Punto p => [p] -> NdTree p
fromList [] = Empty
fromList puntos = aux puntos 0
    where
        aux [] _ = Empty
        aux pts level =
            let
                d = dimension (head pts)
                eje = level `mod` d

                --ORDENO PUNTOS
                ptsordenados = sortBy (compareidx eje) pts
                len = length ptsordenados
                mid = len `div` 2

                --DIVIDO EN l Y R
                izq = take mid ptsordenados --toma mid puntos
                der = drop (mid+1) ptsordenados --dropea mid + 1 puntos, no me quedo la mediana
                raiz = ptsordenados !! mid
            in Node (aux izq level + 1) raiz (aux der level + 1) eje

{-PROBLEMAS DE fromList
No esta chequeado el uso de izq y der si divide bien
creo que !! y sortBy solo toma listas
-}


--Falta la insertar, la copio mna


