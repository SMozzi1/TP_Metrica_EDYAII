import Data.List --Para el sortBy del ej 2

data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
              deriving (Eq, Ord, Show) 


class Punto p where
    dimension :: p -> Int -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada kesima de un punto (comenzando de 0)
    dist :: p -> p -> Double -- calcula la distancia entre dos puntos

---- EJERCICIO 1 ----

-- 1A, GENERICA, USANDO coord Y dimension
distancia :: Punto p => p -> p -> Double
distancia p q | dimension p /= dimension q = -1
         | otherwise = sqrt (sum [(coord i q- coord i p)^2 | i <- [0.. dimension p - 1]])


--1B
newtype Punto2d = P2d (Double, Double) deriving (Show,Eq)
newtype Punto3d = P3d (Double, Double, Double) deriving (Show,Eq)
    
distancia2d :: Punto2d -> Punto2d -> Double
distancia2d (P2d (x1,x2))  (P2d (y1,y2)) = sqrt((y1 - x1)^2 + (y2 - x2)^2)

distancia3d :: Punto3d -> Punto3d -> Double
distancia3d (P3d(x1,x2,x3)) (P3d (y1,y2,y3)) = sqrt((y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2)

-- Instancias (lo hizo Mozzi) (Las dejo para testear, desp hay que borrarlas, chupeton chupeton)

instance Punto Punto2d where
    dimension _ = 2
    coord k (P2d (x, y))
        | k == 0 = x
        | k == 1 = y
        | otherwise = error "Dimension fuera de rango"
    dist = distancia2d


instance Punto Punto3d where
    dimension _ = 3
    coord k (P3d (x, y, z))
        | k == 0 = x
        | k == 1 = y
        | k == 2 = z
        | otherwise = error "Dimension fuera de rango"
    dist  = distancia3d 


---- EJERCICIO 2 ----


--Funcion generica, que toma un indice y dos puntos, y compara sus valores en ese indice
compareidx :: Punto p => Int -> p -> p -> Ordering
compareidx i p q = compare (coord i p) (coord i q)


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



---- EJERCICIO 3 ----


insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p t = insertarRecu p t 0 --insertarRecu nos ayuda a pasar el level actual
  where insertarRecu punto Empty level = Node Empty punto Empty (level `mod` dimension punto)
        insertarRecu punto (Node izq raiz der eje) level
            | coord eje punto < coord eje raiz = Node (insertarRecu punto izq (level + 1)) raiz der eje
            | otherwise = Node izq raiz (insertarRecu punto der (level + 1)) eje


---- EJERCICIO 4 ----


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
    | ejeAlineado == eje = minimumNdTree izq (minP ejeAlineado valor minActual) ejeAlineado --si el eje donde estoy parado es el mismo que el que elimino, solo voy a la izq
    | otherwise = --sino, visito izq y comparo su menor con el de la derecha
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
eliminar _ Empty = Empty --arbol vacio
eliminar punto hoja@(Node Empty valor Empty eje) = if punto == valor then Empty else hoja --caso hoja
eliminar punto (Node izq valor der eje)
       |punto == valor = --Encontre el valor a eliiminar
         if der /= Empty then  --si el subarbol derecho no es vacio
            let
                   nuevoValor = minimumNdTree der valor eje 
                   nuevoDer = eliminar nuevoValor der --elimino el valor copiado, para que no haya dos valores
            in Node izq nuevoValor nuevoDer eje 
         else --si el subarbol derecho es vacio
           let
                   nuevoValor = maximumNdTree izq valor eje
                   nuevoIzq = eliminar nuevoValor izq
            in Node nuevoIzq nuevoValor der eje
        
        --SI LLEGUE ACA, tengo que seguir buscando

       | compareidx eje punto valor == LT = Node (eliminar punto izq) valor der eje --si en el eje actual, el punto a eliminar es menor que donde estoy parado,
                                                                                    -- voy a la izq
       | otherwise = Node izq valor (eliminar punto der) eje --sino, voy a la derecha.

---- EJERCICIO 5 ----

type Rect = (Punto2d, Punto2d)

minMax:: Rect -> (Double,Double,Double,Double)
minMax (P2d (x1,y1), P2d (x2,y2)) = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)


inRegion:: Punto2d -> Rect -> Bool
inRegion (P2d (x,y)) r = x >= minX && x <= maxX && y >= minY && y <= maxY
                        where
                            (minX,maxX,minY,maxY) = minMax r


ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _= []
ortogonalSearch t r =  ortogonalSearchRecu t r [] 
        where
            ortogonalSearchRecu Empty _ list = list --Si el arbol es vacio, retorno la lista que construi hasta el momento
            ortogonalSearchRecu (Node l p r eje) rect list = let
                    (minX,maxX,minY,maxY) = minMax rect --Calculo los minimos y maximos

                    rMin = if eje == 0 then minX else minY --Veo que dimension estoy para ver que rango es, x o y
                    rMax = if eje == 0 then maxX else maxY

                    valNodo = coord eje p --El valor del punto en el eje a comparar

                    in
                        if valNodo < rMin then ortogonalSearchRecu r rect list --Si el valor del punto es menor que el rango min, busco el arbol der
                        else if valNodo > rMax then ortogonalSearchRecu l rect list --Si es mayor que el rango max, busco en el arbol izq
                        else let --Sino, el punto se encuentra en el rango (solo en ese eje)
                                    lisDer = ortogonalSearchRecu r rect list --Veo los puntos del arbol derecho
                                    lisTotal = ortogonalSearchRecu l rect lisDer --lisDer + busqueda en izq = lisTotal
                                    in if inRegion p rect then p:lisTotal else lisTotal --Si el punto esta en la region, lo agrego a la final
                   
                                                            
