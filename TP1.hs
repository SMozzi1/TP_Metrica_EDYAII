import Data.List --Para el sortBy del ej 2
import Control.Monad.Cont (label)

{-- 
Integrantes
    - Santino Mozzi         | DNI: 46427546
    - ALvaro Cabrera        | DNI: 44782260
    - Santiago Salvatico    | DNI: 45341395
--}

-- Representacion de un arbol de puntos n-dimensionales.
data NdTree p = Node (NdTree p) p (NdTree p) Int | Empty
              deriving (Eq, Ord, Show) 

-- Representacion de un punto n-dimensional de un espacio metrico.
class Punto p where
    dimension :: p -> Int       -- devuelve el numero de coordenadas de un punto
    coord :: Int -> p -> Double -- devuelve la coordenada kesima de un punto (comenzando de 0)
    dist :: p -> p -> Double    -- calcula la distancia entre dos puntos

---- EJERCICIO 1 ----
--1A
{--
Toma dos puntos y calcula su distancia
Notar que es generica, esto es, que Punto puede ser de cualquier tipo.
Trabaja con las funciones declaradas en su clase
--}
distancia :: Punto p => p -> p -> Double
distancia p q | dimension p /= dimension q = -1
         | otherwise = sqrt (sum [(coord i q- coord i p)^2 | i <- [0.. dimension p - 1]])


--1B
-- Tipos de datos para representar puntos 2-dimensionales y 3-dimensionales.
newtype Punto2d = P2d (Double, Double) deriving (Show,Eq)
newtype Punto3d = P3d (Double, Double, Double) deriving (Show,Eq)

{-
Calcula la distancia de dos puntos 2-dimensionales.
-}
distancia2d :: Punto2d -> Punto2d -> Double
distancia2d (P2d (x1,x2))  (P2d (y1,y2)) = sqrt((y1 - x1)^2 + (y2 - x2)^2)

{-
Calcula la distancia de dos puntos 3-dimensionales.
-}
distancia3d :: Punto3d -> Punto3d -> Double
distancia3d (P3d(x1,x2,x3)) (P3d (y1,y2,y3)) = sqrt((y1 - x1)^2 + (y2 - x2)^2 + (y3 - x3)^2)

-- Instancia de Punto para puntos 2-dimensionales.
instance Punto Punto2d where
    dimension _ = 2
    coord k (P2d (x, y))
        | k == 0 = x
        | k == 1 = y
        | otherwise = error "Dimension fuera de rango"
    dist = distancia2d

-- Instancia de Punto para puntos 3-dimensionales.
instance Punto Punto3d where
    dimension _ = 3
    coord k (P3d (x, y, z))
        | k == 0 = x
        | k == 1 = y
        | k == 2 = z
        | otherwise = error "Dimension fuera de rango"
    dist  = distancia3d 


---- EJERCICIO 2 ----
{-
Auxiliar. Toma un indice y dos puntos, y compara sus valores en ese indice.
Devuelve:
        - LT si la coordenada (dada por el indice) del primer punto es menor a la del segundo.
        - EQ si las coordenadas son iguales.
        - GT en otro caso.
-}
compareidx :: Punto p => Int -> p -> p -> Ordering
compareidx i p q = compare (coord i p) (coord i q)

{-
Toma una lista de puntos y los transforma en un arbol de puntos. (complejidad n log^2 n)
-}
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

{-
Recibe un punto, un arbol, e inserta el punto en el arbol.
Compara en cada nivel el eje actual de la raiz actual con el punto.
Si es menor, insertar en el subarbol izquierdo,
en caso contrario, inserta en el subarbol derecho.
-}
insertar :: Punto p => p -> NdTree p -> NdTree p
insertar p t = insertarRecu p t 0 --insertarRecu nos ayuda a pasar el level actual
  where insertarRecu punto Empty level = Node Empty punto Empty (level `mod` dimension punto)
        insertarRecu punto (Node izq raiz der eje) level
            | coord eje punto < coord eje raiz = Node (insertarRecu punto izq (level + 1)) raiz der eje
            | otherwise = Node izq raiz (insertarRecu punto der (level + 1)) eje


---- EJERCICIO 4 ----


{-
Auxiliar. Dados dos puntos p1 y p2, devuelve el minimo segun el eje indicado.
-}
minP :: (Punto p) => Int -> p -> p -> p
minP eje p1 p2 = if compareidx eje p1 p2 == LT then p1 else p2

{-
Auxiliar. Dados dos puntos p1 y p2, devuelve el maximo segun el eje indicado.
-}
maxP :: (Punto p) => Int -> p -> p -> p
maxP eje p1 p2 = if compareidx eje p1 p2 == GT then p1 else p2

{-
Auxiliar. Recibe un arbol NdTree, un punto p como ultimo valor minimo encontrado, y un eje,
la funcion retornara el minimo punto encontrado en el arbol visto unicamente en la coordenada eje.
En cada paso se compara el valor del nodo con el ultimo valor minimo encontrado, y actualiza este ultimo
si es necesario.
-}
minimumNdTree :: Punto p => NdTree p -> p -> Int -> p
minimumNdTree Empty minActual _ = minActual
minimumNdTree (Node izq valor der eje) minActual ejeAlineado
    | ejeAlineado == eje = minimumNdTree izq (minP ejeAlineado valor minActual) ejeAlineado --si el eje donde estoy parado es el mismo que el que elimino, solo voy a la izq
    | otherwise = --sino, visito izq y comparo su menor con el de la derecha
        let minFinal = minimumNdTree izq (minP ejeAlineado valor minActual) ejeAlineado
        in minimumNdTree der minFinal ejeAlineado

{-
Auxiliar. Recibe un arbol NdTree, un punto p como ultimo valor maximo encontrado, y un eje,
la funcion retornara el maximo punto encontrado en el arbol visto unicamente en la coordenada eje.
En cada paso se compara el valor del nodo con el ultimo valor maximo encontrado, y actualiza este ultimo
si es necesario.
-}
maximumNdTree :: Punto p => NdTree p -> p -> Int -> p
maximumNdTree Empty maxActual _ = maxActual
maximumNdTree (Node izq valor der eje) maxActual ejeAlineado
    | ejeAlineado == eje = maximumNdTree der (maxP ejeAlineado valor maxActual) ejeAlineado
    | otherwise =
        let maxFinal = maximumNdTree der (maxP ejeAlineado valor maxActual) ejeAlineado
        in maximumNdTree izq maxFinal ejeAlineado

{-
Dado un punto y un arbol de puntos, elimina el punto del arbol.
Notar que una vez eliminado, si es una hoja, termina.
En caso de que el nodo tenga hijos:
    - si tiene hijo derecho, busca un punto para copiar en su posicion,
    para luego eliminar el punto original del subarbol derecho.
    - si no tiene hijo derecho, repite el comportamiento que en el caso
    de que tenga hijo derecho, pero buscando el maximo del subarbol izquierdo.
-}
eliminar :: (Eq p, Punto p) => p -> NdTree p -> NdTree p
eliminar _ Empty = Empty --arbol vacio
eliminar punto hoja@(Node Empty valor Empty eje) = if punto == valor then Empty else hoja --caso hoja
eliminar punto (Node izq valor der eje)
    |punto == valor = --Encontre el valor a eliminar
        case der of
-- Separo en los casos del hijo derecho,
-- ya que es el mas importante, y a esta altura de
-- la funcion tengo garantizado que al menos un hijo existe. 
            Node _ vd _ _ -> let -- Si existe el hijo derecho, busco un reemplazo de mi raiz en su subarbol.
                nuevoValor = minimumNdTree der vd eje 
                nuevoDer = eliminar nuevoValor der --elimino el valor copiado, para que no haya dos valores
                in Node izq nuevoValor nuevoDer eje 
            Empty -> let -- Si no existe mi hijo derecho, obligatoriamente el izquierdo existe.
                Node _ vi _ _ = izq
                nuevoValor = maximumNdTree izq vi eje
                nuevoIzq = eliminar nuevoValor izq
                in Node nuevoIzq nuevoValor der eje
        --SI LLEGUE ACA, tengo que seguir buscando
    | compareidx eje punto valor == LT = Node (eliminar punto izq) valor der eje --si en el eje actual, el punto a eliminar es menor que donde estoy parado,
                                                                                    -- voy a la izq
    | otherwise = Node izq valor (eliminar punto der) eje --sino, voy a la derecha.

---- EJERCICIO 5 ----
type Rect = (Punto2d, Punto2d)

{-
Auxiliar, calcula los min y max de un rectangulo tipo Rect.
-}
--5A
minMax:: Rect -> (Double,Double,Double,Double)
minMax (P2d (x1,y1), P2d (x2,y2)) = (min x1 x2, max x1 x2, min y1 y2, max y1 y2)

{-Recibe un punto y un rectangulo, y determina si
el punto esta dentro del area del rectangulo-}
inRegion:: Punto2d -> Rect -> Bool
inRegion (P2d (x,y)) rect = x >= minX && x <= maxX && y >= minY && y <= maxY
                        where
                            (minX,maxX,minY,maxY) = minMax rect

--5B
{-
Toma un arbol de puntos, un rectangulo, y devuelve una lista,
la cual contiene los puntos que pertenecen al area del rectangulo.
-}
ortogonalSearch :: NdTree Punto2d -> Rect -> [Punto2d]
ortogonalSearch Empty _= []
ortogonalSearch t r =  ortogonalSearchRecu t r [] 
    where
        ortogonalSearchRecu Empty _ list = list --Si el arbol es vacio, retorno la lista que construi hasta el momento
        ortogonalSearchRecu (Node l p r eje) rect list =
            let
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


main :: IO ()
main = do
    let
        -- Puntos 2D
        p1 = P2d (2,3)
        p2 = P2d (5,4)
        p3 = P2d (9,6)
        p4 = P2d (4,7)
        p5 = P2d (8,1)
        p6 = P2d (7,2)

        puntos = [p1,p2,p3,p4,p5,p6]

        -- Construccion del arbol
        arbol = fromList puntos

    putStrLn "===== ARBOL ORIGINAL ====="
    print arbol

    putStrLn "\n===== DISTANCIAS ====="
    print (distancia p1 p2)
    print (distancia p3 p4)

    putStrLn "\n===== INSERTAR ====="
    let
        nuevoPunto = P2d (6,5)
        arbolInsertado = insertar nuevoPunto arbol

    print arbolInsertado

    putStrLn "\n===== ELIMINAR ====="
    let
        arbolEliminado = eliminar p2 arbolInsertado

    print arbolEliminado

    putStrLn "\n===== MINIMO EN EJE X ====="
    print (minimumNdTree arbol p1 0)

    putStrLn "\n===== MAXIMO EN EJE Y ====="
    print (maximumNdTree arbol p1 1)

    putStrLn "\n===== BUSQUEDA ORTOGONAL ====="
    let
        rect = (P2d (3,2), P2d (8,6))

    print (ortogonalSearch arbol rect)
