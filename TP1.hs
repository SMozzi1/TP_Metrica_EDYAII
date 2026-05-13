import Data.List --Para el sortBy del ej 2

data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
              deriving (Eq, Ord, Show) 


class Punto p where
    dimension :: p -> Int -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada kesima de un punto (comenzando de 0)
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos

-- 1A, GENERICA, USANDO coord Y dimension
distancia :: Punto p => p -> p -> Double
distancia p q | dimension p /= dimension q = -1
         | otherwise = sqrt (sum [(coord i q- coord i p)^2 | i <- [0.. dimension p - 1]])


--1B
newtype Punto2d = P2d (Double, Double)
newtype Punto3d = P3d (Double, Double, Double)
    
distancia2d :: Punto2d -> Punto2d -> Double
distancia2d (P2d (x1,x2))  (P2d (y1,y2)) = sqrt((y1 - x1)^2 + (y2 - x2)^2)

distancia3d :: Punto3d -> Punto3d -> Double
distancia3d (P3d(x1,x2,x3)) (P3d (y1,y2,y3)) = sqrt((y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2)

-- Instancias (lo hizo Mozzi)

instance Punto Punto2d where
    dimension _ = 2
    coord k (P2d (x, y))
        | k == 0 = x
        | k == 1 = y
        | otherwise = error "Dimension fuera de rango"
    dist p q = distancia2d p q


instance Punto Punto3d where
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
  where insertarRecu punto Empty level = Node Empty punto Empty (level `mod` dimension punto)
        insertarRecu punto (Node izq raiz der eje) level
            | coord eje punto < coord eje raiz = Node (insertarRecu punto izq (level + 1)) raiz der eje
            | otherwise = Node izq raiz (insertarRecu punto der (level + 1)) eje


-- Ejercicio 4 (Mozzi) --
{- Pola: Hice funciones para calcular el maximo y el minimo de un arbol,
    despues las vamos a poder usar en la funcion eliminar para buscar el 
    nodo "candidato" a reemplazar la raiz -}

{-- Dados dos puntos p1 y p2, devuelve el minimo segun el eje indicado. --}
minP :: (Punto p) => Int -> p -> p -> p
minP eje p1 p2 = if compareidx eje p1 p2 == LT then p1 else p2

{-- Dados dos puntos p1 y p2, devuelve el maximo segun el eje indicado. --}
maxP :: (Punto p) => Int -> p -> p -> p
maxP eje p1 p2 = if compareidx eje p1 p2 == GT then p1 else p2

{-- Recibe un arbol NdTree, un punto p como ultimo valor minimo encontrado, y un eje,
la funcion retornara el minimo punto encontrado en el arbol visto unicamente en la coordenada eje. --}
minimumNdTree :: Punto p => NdTree p -> p -> Int -> p
minimumNdTree Empty minActual _ = minActual
minimumNdTree (Node izq valor der eje) minActual ejeAlineado
    | ejeAlineado == eje = minimumNdTree izq (minP ejeAlineado valor minActual) ejeAlineado
    | otherwise =
    	let minFinal = minimumNdTree izq (minP ejeAlineado valor minActual) ejeAlineado
    	in minimumNdTree der minFinal ejeAlineado


{-- Recibe un arbol NdTree, un punto p como ultimo valor maximo encontrado, y un eje,
la funcion retornara el maximo punto encontrado en el arbol visto unicamente en la coordenada eje. --}
maximumNdTree :: Punto p => NdTree p -> p -> Int -> p
maximumNdTree Empty maxActual _ = maxActual
maximumNdTree (Node izq valor der eje) maxActual ejeAlineado
    | ejeAlineado == eje = maximumNdTree der (maxP ejeAlineado valor maxActual) ejeAlineado
    | otherwise =
        let maxFinal = maximumNdTree der (maxP ejeAlineado valor maxActual) ejeAlineado
        in maximumNdTree izq maxFinal ejeAlineado


eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar _ Empty = Empty
eliminar punto hoja@(Node Empty valor Empty eje) = if punto == valor then Empty else hoja
eliminar punto (Node izq valor der eje)
	   |(punto == valor) && (der /= Empty) = Node izq nuevoValor nuevoDer eje
	   	where
	   		nuevoValor = minimumNdTree der valor eje
	   		nuevoDer = eliminar nuevoValor der

	   |(punto == valor) && (der == Empty) = Node nuevoIzq nuevoValor der eje
	   	where
	   		nuevoValor = maximumNdTree izq valor eje
	   		nuevoIzq = eliminar nuevoValor izq

	   | compareidx eje punto valor == LT = Node (eliminar punto izq) valor der eje
	   | otherwise = Node izq valor (eliminar punto der) eje

{-
Si estoy en el nodo a eliminar y es una hoja, devuelvo empty, ya que despues los datos que no son usados se borran automaticamente
Si estoy en el nodo a eliminar y tiene hijo derecho, modifico unicamente su rama derecha,
-}


-- ej 5 --

type Rect = (Punto2d, Punto2d)

inRegion:: Punto2d -> Rect -> Bool
inRegion _ (0,0) = False
inRegion (P2d (x,y)) (p1, p2) = x >= minX && x <= maxX && y >= minY && y <= maxY
                        where
                            minX = min (coord 0 p1) (coord 0 p2)
                            maxX = max (coord 0 p1) (coord 0 p2)

                            minY = min (coord 1 p1) (coord 1 p2)
                            maxY = max (coord 1 p1) (coord 1 p2)


ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _ = []
ortogonalSearch t (p1, p2) =  ortogonalSearchRecu t (p1,p2) [] 
        where
            ortogonalSearchRecu Empty _ list = list
            ortogonalSearchRecu (Node l p r eje) (p1, p2)  list = let
                    minX = min (coord 0 p1) (coord 0 p2)
                    maxX = max (coord 0 p1) (coord 0 p2)

                    minY = min (coord 1 p1) (coord 1 p2)
                    maxY = max (coord 1 p1) (coord 1 p2)

                    x = coord 0 p
                    y = coord 1 p

                    rMin = if eje == 0 then minX else minY 
                    rMax = if eje == 0 then maxX else maxY

                    valNodo = coord eje p 

                    in case () of
                    |   valNodo < rMin -> ortogonalSearchRecu r (p1,p2)  list
                    |   valNodo > rMax -> ortogonalSearchRecu l (p1, p2) list
                    |   otherwise -> let
                                    lisDer = ortogonalSearchRecu r (p1,p2) list

                                    lisTotal = ortogonalSearchRecu l (p1, p2) lisDer

                                    in if inRegion p (p1, p2) then (p:lisTotal) else lisTotal

             
                                                                    
                                                            
