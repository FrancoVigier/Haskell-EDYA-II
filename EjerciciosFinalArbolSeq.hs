module PracticaTres where

import Data.List 

import qualified Arr as A
import Arr((!))

import Seq
import ArrSeq
import Par((|||))

data Tree a = E | L a | N Int (Tree a) (Tree a) deriving Show
--___________________________________________________________________________________________________________________________________________________________________________
--Ejercicio Final 1. SWIRTZ
split :: ([Char]  -> Bool ) ->Tree [Char]  -> (Tree (Int, [Char]), Tree(Int, [Char]))
split f E = (E,E)
split f t = splitaux f t 0

splitaux::([Char]  -> Bool ) ->Tree [Char]  -> Int  -> (Tree (Int, [Char]), Tree(Int, [Char]))
splitaux f E carryLeaf = (E, E)
splitaux f (L a) carryLeaf = if (f a) then (L (carryLeaf, a), E) else (E, L(carryLeaf, a))
splitaux f (N s l r) carryLeaf = let  
                                                                (arbolBuenoL, arbolMaloL) = splitaux f l carryLeaf  --1 
                                                                (arbolBuenoR, arbolMaloR) = splitaux f r (carryLeaf + (cantLeaf l)) --1
                                                                nl = combinar arbolBuenoL arbolBuenoR --2
                                                                nr = combinar arbolMaloL arbolMaloR --2
                                                           in (nl,nr)

--Los que tienen 1 se pueden combinar con ||| y los que tienen dos se pueden combinar con |||

combinar :: Tree a  -> Tree a  -> Tree a --O h
combinar E t = t
combinar t E = t --Agrego casos del E
combinar (L a) t = N (1 + size t) (L a) t
combinar (N s l E) t = N (s + size t) l t--Agrego casos del E
combinar (N s l r) t = N (size t + size l + size r) (combinar l r)  t


size :: Tree a  -> Int --O 1
size E = 0
size (L a) = 1
size (N s _ _) = s

cantLeaf ::  Tree a -> Int
cantLeaf E = 0
cantLeaf (L a) = 1
cantLeaf (N s l r) = let
                                           lL = cantLeaf l --1
                                           lR = cantLeaf r --1
                                     in lR + lL

palindromo :: [Char]  -> Bool
palindromo xs = (xs == reverse xs)

arbolTest = (N 8 (N 2 (L "oso") (L "casa"))  (N 3 (N 2 (L "reconocer") (L "pala")) (L "ala")) )

tabulateT :: (Int -> a) -> Int -> Tree a
tabulateT f 0 = E
tabulateT f n = tabulateaux f  n 0 n

tabulateaux ::(Int -> a) -> Int -> Int-> Int -> Tree a
tabulateaux f 0 carry b = if (b > carry) then L(f carry) else E
tabulateaux f n carry b | (mod n 2) /= 0 = let
                                                                                        l = tabulateaux f (div n 2) carry b
                                                                                        r = tabulateaux f (div n 2) (div n 2 + carry +1) b
                                                                                   in g l r
                                              | (mod n 2) == 0 = let
                                                                                        l = tabulateaux f (div n 2) carry b
                                                                                        r = tabulateaux f (div n 2 -1) (div n 2 + carry + 1) b
                                                                                  in g l r
                                                                        where g l r| (size l)==0 && (size r)==0 = E
                                                                                              | (size l) == 0 = r
                                                                                              | (size r) == 0 = l
                                                                                              | otherwise = N (size l + size r + 1) l r


fromSlow :: Int -> Int-> Int -> Tree Int
fromSlow n m k = tabulateT (fSaux n k) m

fSaux :: Int -> Int -> Int -> Int
fSaux n k index = n + (div index k)

--if (size l == 0 && size r == 0) then (L (f (div n 2 + carry))) else (N n l r)
--___________________________________________________________________________________________________________________________________________________________________________
--Ejercicio Final 2 PETRU
inorden :: Tree a -> [a]
inorden E = []
inorden (L a) = [a]
inorden (N s l r) = inorden l ++ inorden r
--a) Para este apartado recorda que la cantidad de hojas de la izq mas el acarreo es mi indice
divide :: Tree a -> Int -> (Tree a, Tree a)

divide E n = (E,E) --Divide de trabajo O( size t) y profundidad O (h)
divide t 0 = (E,t)
divide t n = divideaux t n 0

divideaux :: Tree a -> Int ->Int ->(Tree a, Tree a)
divideaux E index carryLeaf = (E,E)
divideaux (L a) index carryLeaf = if index > carryLeaf then (L a, E) else (E, L a)
divideaux (N s l r) index carryLeaf = let
                                                                              cantL = cantLeaf l
                                                                              ((aL,dL),(aR,dR)) = (divideaux l index carryLeaf) ||| (divideaux r index (carryLeaf + cantL))
                                                                              in (g aL aR,g dL dR) 
                                                                        where g l r| (size l)==0 && (size r)==0 = E
                                                                                              | (size l) == 0 = r
                                                                                              | (size r) == 0 = l
                                                                                              | otherwise = N (size l + size r) l r


testDivide :: Tree a -> Int -> ([a],[a])
testDivide t n= let
                                (a,d) =  divide t n
                                in (inorden a, inorden d) 


divideLineal :: Tree a -> Int -> (Tree a, Tree a) -- Divide con trabajo O(h) y profundidad O(h)
divideLineal E n = (E,E)
divideLineal t 0 = (E,t)
divideLineal t n = divideauxLineal t n 0


divideauxLineal :: Tree a -> Int ->Int ->(Tree a, Tree a)
divideauxLineal E index carryLeaf = (E,E)
divideauxLineal (L a) index carryLeaf = if index > carryLeaf then (L a, E) else (E, L a)
divideauxLineal (N s l r) index carryLeaf = let
                                                                                          cantL = cantLeaf l
                                                                                     in if (cantL > index) then let
                                                                                                                                             (preindexL,postindexL) = divideauxLineal l index carryLeaf
                                                                                                                                        in (preindexL, N (size postindexL + size r) postindexL r)
                                                                                          else if(index > cantL) then let
                                                                                                                                                      (preindexR,postindexR) = divideauxLineal r index (carryLeaf + cantL)
                                                                                                                                                 in (N (cantL + size preindexR) l preindexR,postindexR)
                                                                                                    else (l,r)

testDivideLineal :: Tree a -> Int -> ([a],[a])
testDivideLineal t n= let
                                (a,d) =  divideLineal t n
                                in (inorden a, inorden d) 

--b) Recorda que para este ej y el siguiente comparas cantidad de elementos y extraes o no de la otra rama si es necesario...
interleaves :: Tree a -> Tree a -> Tree a -- O((maxH s1 s2)^2)
interleaves E s2 = s2
interleaves s1 E = s1
interleaves s1 s2 = let
                                             ((heads1, tails1),(heads2,tails2)) = (divide s1 1) ||| (divide s2 1)
                                             intercalados  = interleaves tails1 tails2
                                      in N (1 + 1 + size intercalados) (N 2 heads1 heads2) intercalados

arbolX = (N 4 (N 2 (L "x0") (L "x1"))(N 2 (L "x2")(L "x3")))
arbolY = (N 3 (L "x0") (N 2 (L "y1") (L "y2")))

interleavesLineal  :: Tree a -> Tree a -> Tree a --O( h)
interleavesLineal E s2 = s2
interleavesLineal s1 E = s1
interleavesLineal s1@(L a) s2 = let
                                                                     (head2,tail2) = divide s2 1
                                                               in N (size s2 + 1) (N 2 (L a) head2) tail2
interleavesLineal s1 s2@(L a) = let
                                                                     (head1,tail1) = divide s1 1
                                                               in N (size s1 + 1) (N 2 head1 (L a)) tail1
interleavesLineal s1@(N sa l1 r1) s2@(N sb l2 r2) = let
                                                                                                            (cl1,cl2) = cantLeaf l1 ||| cantLeaf l2
                                                                                                      in if (cl1 > cl2) then let
                                                                                                                                                     dif = cl1 - cl2
                                                                                                                                                     (nr2, tailnr2) = divide r2 dif -- necesito extraer la diferencia de la otra rama
                                                                                                                                                     (nL,nR) = interleaves l1 (N (size l2 + size nr2) l2 nr2) ||| interleaves r1 tailnr2
                                                                                                                                              in   N (size nL + size nR) nL nR --Si tengo mas elementos en la izquierda l1 entonces necesito de todo s2 para intercalar y r1 nunca va a intercalar con nadie
                                                                                                                                   else if (cl2  > cl1) then  let
                                                                                                                                                                                        dif = cl2 - cl1
                                                                                                                                                                                        (nr1, tailnr1) = divide r1 dif
                                                                                                                                                                                        (nL,nR) = interleaves (N (size l1 + size nr1) l1 nr1) l2 ||| interleaves tailnr1 r2
                                                                                                                                                                                  in   N (size nL + size nR) nL nR
                                                                                                                                                                      else let (nL,nR) = interleavesLineal l1 l2 ||| interleavesLineal r1 r2
                                                                                                                                                                                in (N (size nL+ size nR) nL nR)

--___________________________________________________________________________________________________________________________________________________________________________
--Ejercicio Final 3 LUCHO
zipT :: Tree a -> Tree b -> Tree (a,b)
zipT E s2 = E
zipT s1 E = E
zipT (L a) (L b) = L (a,b)
zipT s1@(L a) s2 = let
                                         (head2,tail2) = divide s2 1
                                     in L (a, sacardato head2)
zipT s1 s2@(L a) = let
                                          (head1,tail1) = divide s1 1
                                     in L (sacardato head1,a)
zipT s1@(N sa l1 r1) s2@(N sb l2 r2) = let
                                                                                  (cl1,cl2) = cantLeaf l1 ||| cantLeaf l2
                                                                           in if (cl1 > cl2) then let
                                                                                                                         dif = cl1 - cl2
                                                                                                                         (nr2, tailnr2) = divide r2 dif -- necesito extraer la diferencia de la otra rama
                                                                                                                         (nL,nR) = zipT l1 (N (size l2 + size nr2) l2 nr2) ||| zipT r1 tailnr2
                                                                                                                   in N (size nL + size nR) nL nR --Si tengo mas elementos en la izquierda l1 entonces necesito de todo s2 para intercalar y r1 nunca va a intercalar con nadie
                                                                                  else if (cl2  > cl1) then  let
                                                                                                                                      dif = cl2 - cl1
                                                                                                                                      (nr1, tailnr1) = divide r1 dif
                                                                                                                                      (nL,nR) = zipT (N (size l1 + size nr1) l1 nr1) l2 ||| zipT tailnr1 r2
                                                                                                                                 in   N (size nL + size nR) nL nR
                                                                                             else let (nL,nR) = zipT l1 l2 ||| zipT r1 r2
                                                                                                       in (N (size nL+ size nR) nL nR)


sacardato::Tree a ->a
sacardato (L a) = a
  

--Ejercicio 4 LUCHO
data Heap a = EG | NG a [Heap a]

minHijosHeap :: Ord a => [Heap a] -> a  --Funcion Parcial Siempre con Heap con un elemento
minHijosHeap [(NG a [])] = a
minHijosHeap ((NG a _):hermanos) = min a (minHijosHeap hermanos)

isMinHeap :: Ord a => Heap a -> Bool
isMinHeap EG = True
isMinHeap (NG padre hijos) = (minHijosHeap hijos > padre) && foldr (&&) True (map isMinHeap hijos) --Y tengo que reducir eso foldr && (mapea a todos los hijos del padre y cada hijo lo mapea a todos sus hijos)

--___________________________________________________________________________________________________________________________________________________________________________
--Ejercicio 5 MESSU

gananciaVenta:: A.Arr (Float, Float)  -> Float  -> Float
gananciaVenta s x = let 
                                              seqComprayFuturasVentas = tabulateA (\index -> (extraerCompra (s!(index)),reduceA maximaVenta (0.0,0.0) (dropA s (index + 1))) ) (lengthA s)
                                              seqGananciasSufijos = mapA (calcularGanancia x) seqComprayFuturasVentas
                                         in  reduceA max 0.0 seqGananciasSufijos

maximaVenta::(Float, Float)  ->(Float, Float)  -> (Float, Float)
maximaVenta (ca,a) (cb,b) = if a >= b then (ca,a) else (cb,b) 

minimaCompra::(Float, Float)  ->(Float, Float)  -> (Float, Float)
minimaCompra a (0.0,0.0) = a --tope para el caso base e
minimaCompra (0.0,0.0) b = b --tope para el caso base e
minimaCompra (ca,a) (cb,b) = if ca >= cb then (cb,b) else (ca,a) 

calcularGanancia ::Float  -> (Float, (Float,Float))  ->  Float
calcularGanancia x (c,(_,v)) = (x/c) * (v-c)

extraerCompra:: (Float, Float)  ->Float
extraerCompra (c,_) =  c

extraerVenta:: (Float, Float)  ->Float
extraerVenta (_,v) =  v

sacarGanancia ::Float  -> (Float,Float)  ->  Float
sacarGanancia x (c,v) = (x/c) * (v-c)

-- En el anterior muy parecido al ej de YPF, creo una seq (Compra, MaximaVentaFutura*), le mapeo la funcion calcularGanancia y extraigo el maximo
-- *por cuestiones de tipo MaximaVentaFutura = (_ , maximaventafutura)
--Podemos ver que en Costos de S esta en log |s| acotada, pero en W (|s|^2) cosa que no puede ser. Vamos a implementar una con W |s|
gananciaVentaLineal:: A.Arr (Float, Float)  -> Float  -> Float -- W |s| y S log |s|
gananciaVentaLineal s x = let
                                                           (seqminCompras, minCompra) = scanA minimaCompra (0.0,0.0) s --Seq (c,_). Se puede paralelizar con el de abajo
                                                           (seqmaxVentas, maxVenta) = scanA maximaVenta (0.0,0.0) (reverseA s) --Seq (_,v). Se puede paralelizar con el de arriba
                                                           (seqMinCompras,seqMaxVentas) = concatA (dropA seqminCompras 1) (singletonA minCompra) ||| concatA (dropA seqmaxVentas 1) (singletonA maxVenta)   --  Les saco el (0.0,0.0) de base a las seq porque sino me dan infinity  y le pongo el ultimo valor
                                                           seqGanancias = tabulateA (\index -> sacarGanancia x ((extraerCompra (seqMinCompras ! index)) , (extraerVenta (seqMaxVentas ! (lengthA s - index -1))))) (lengthA s)
                                                           (seqmaxGanancias, maxGanancia) = scanA max 0.0 seqGanancias
                                                     in maxGanancia
reverseA :: A.Arr a -> A.Arr a
reverseA s = tabulateA (\y  -> s ! (lengthA s - y - 1)) (lengthA s)
      
--Ejercicio 6 MESSU
appendIndex:: Int -> Tree a -> Tree a -> Tree a
appendIndex n E s2 = s2
appendIndex n s1 E = s1
appendIndex n s1 s2 = appendIndexaux n s1 s2 0

appendIndexaux :: Int -> Tree a -> Tree a ->Int ->Tree a -- si el W(divide) es O(h) entonces W(appendIndex) es O(h)
appendIndexaux index E s2 carryLeaf = s2
appendIndexaux index s1 E carryLeaf = s1
appendIndexaux index s1@(N s l r) s2 carryLeaf = let
                                                                                                          (arbolL,arbolR) = divide s1 index
                                                                                                     in if (size arbolR == 0) then (N (size arbolL + size s2) arbolL s2)
                                                                                                                                                  else (N (s + size s2) (N (size arbolL + size s2) arbolL s2) arbolR) 
--Ejercicio 7 MESSU
--a)
--Estas son especificaciones de las operaciones, despues en la implementacion con las particularidades de la Estructura elegida TE LA ARREGLAS
--No puedo especificar el comportamiento con la tabla de un solo elemento ni con la de 0 elementos. Con Maybe se arreglarioa para hacerla total, sabemos que las K estan ordenadas
--lookup key (put (K,V) (put (K',V') empty)) = if (key \= K) then (lookup key (put (K',V') empty)) else V
--lookup key (put (K,V) (put (K',V') table)) = if (key \= K) then (lookup key (put (K',V') table)) else V

--erase key empty = empty
--erase key (put (K,V) empty) = if (key == K) then empty else (put (K,V) empty)
--erase key (put (K,V) (put (K',V') table)) = if(key == K) then (put (K',V') table) else put (K,V) (erase key (put (K',V') table))

--b)
data BST k v = ET | NT (BST k v) (k,v) (BST k v)

put :: Ord a => (a, b) ->  BST a b -> BST a b
put (a,b) ET = NT ET (a,b) ET
put (a,b) (NT l (k,v) r) | (a == k) = NT l (a,b) r
                                            | (a > k) = NT l (k,v) (put (a,b) r)
                                            | (k > a) = NT (put (a,b) l) (k,v) r

--___________________________________________________________________________________________________________________________________________________________________________
-- 03/12/2021
--Ej 2
type Destino = [Char]
type Precio = Integer
type Fecha = [Char]

fecha :: A.Arr (Destino,Precio,Fecha) -> Destino -> Precio -> Maybe Fecha  --S(lg|s|)
fecha s d p | (lengthA s == 0) = Nothing
                       | otherwise = let
                                                          seqD = filterA (\(d1,p1,f1) -> d == d1) s --S(lg|s|)
                                                          (sumPreciosdeD,ultimaSumadeD)  = scanA (\(d1,p1,f1) (d2,p2,f2)-> (d1,p1+p2,f2)) (d,0,"") seqD --S(lg|s|)
                                                          seqSinBase = concatA (dropA sumPreciosdeD 1) (singletonA ultimaSumadeD) --S(1)
                                                          seqPredicadoOk = filterA (\(d1,p1,f1) -> (p1 >= p)) seqSinBase --S(lg |s|)
                                                    in if (lengthA seqPredicadoOk == 0) then Nothing else (Just ((\(_,_,f1) -> f1) (seqPredicadoOk ! 0))) --S(1)

testFecha = fromListA[("Ros",22,"01-09"),("Hab",30,"01-09"),("BsAs",33,"03-09"),("Hab",40,"03-09")]
testFecha2 = fromListA[("Ros",22,"01-09"),("Hab",30,"01-09"),("Hab",25,"02-09"),("BsAs",33,"03-09"),("Hab",40,"03-09")]

--Ej 1, AVL
data AVL a = EAVL | NAVL Int (AVL a) a (AVL a) deriving Show

--a)
heigth :: AVL a  -> Int
heigth EAVL = 0
heigth (NAVL h l x r) = let
                                                   (hl,hr) = heigth l ||| heigth r
                                             in 1 + max hl hr
--b)
extractHeightDiference :: AVL a  -> Int
extractHeightDiference EAVL = 0
extractHeightDiference (NAVL h _ _ _) = h

deleteMax :: Ord a => AVL a  -> (a, AVL a) --Funcion Parcial
deleteMax (NAVL h EAVL x EAVL) = (x,EAVL)
deleteMax (NAVL h l x EAVL) = (x, l)
deleteMax (NAVL h l x r) = let
                                                            (max, tree) = deleteMax r
                                                            (hTree, hl) = extractHeightDiference tree ||| extractHeightDiference l
                                                      in (max, NAVL (hl + hTree) l x tree)
--c)
deleteAVL:: Ord a => a -> AVL a -> AVL a
deleteAVL a EAVL = EAVL
deleteAVL a t@(NAVL h l x EAVL) = if x == a then l else t
deleteAVL a t@(NAVL h l x r) |  (x == a)  = let
                                                                                 (max, nR) = deleteMax r
                                                                                 (hL,hnR) = extractHeightDiference l ||| extractHeightDiference nR
                                                                            in balance (NAVL (hL + hnR) l x nR) --como eliminamos de la rama DER, esta es la que se me desbalancea...En balance solo voy a usar los casos de desbalance derecho(balanceR)
                                                   | (a > x) = let
                                                                             nR = deleteAVL a r
                                                                             (hL,hnR) = extractHeightDiference l ||| extractHeightDiference nR
                                                                       in balance(NAVL (hL + hnR) l x nR)  --como eliminamos de la rama DER, esta es la que se me desbalancea...En balance solo voy a usar los casos de desbalance derecho(balanceR)
                                                   | (x > a) = let
                                                                            nL = deleteAVL a l
                                                                            (hR,hnL) = extractHeightDiference r ||| extractHeightDiference nL
                                                                       in balance(NAVL (hR + hnL) nL x r)  --como eliminamos de la rama IZQ, esta es la que se me desbalancea...En balance solo voy a usar los casos de desbalance izquierdo(balanceL)

balance :: AVL a-> AVL a -- Ya dada por la catedra asi que para que compile el code hago la identidad, PERO CABE ACLARAR QUE como almacenamos el slope (heigth l - heigth r) en cada NODO el rebalance es O(1)
balance t = t
--d) Propiedad a demostrar: si t es AVL entonces deleteAVL x t es un AVL
--     Sea t = NAVL h l a r. Asumo que l y r son tambien AVL's (Por lo tanto por def AVL, todo AVL es BST)

--     Probemos la propiedad para el caso BASE:
--     deleteAVL x EAVL = {deleteAVL .1} EAVL. Por definicion el arbol vacio esta balanceado, por lo tanto es un AVL

--     Probemos la propiedad para t = NAVL h l a r. 
--     HI1) deleteAVL x l es un AVL, y tambien vale para cualquier subarbol de l
--     HI2) deleteAVL x r es un AVL, y tambien vale para cualquier subarbol de r
--     deleteAVL x t@(NAVL h l a r) = {deleteAVL .3, caso x = a}
--                                                                                                                      (max,nR) = deleteMax r --> Todo AVL es un BST, por lo tanto r es un BST.  LEMA AUX 1: si t es un BST entonces deleteMax t es un BST. nR es un BST
--                                                                                                                      (hL,hnR) = extractHeightDiference l ||| extractHeightDiference nR --> Se puede demostrar, pero no es necesario para el ejercicio
--                                                                                                                 in balance t1@(NAVL _ l a nR)  --> Tenemos que demostrar que t1 es un BST, para eso tenemos que determinar que cumple con las invariantes de los BST
--                                                                                                                      1. sabemos que l es un BST (por dato) y que (NAVL l a EAVL) es un BST (por dato) 
--                                                                                                                      2. nR es un BST por el LEMA AUX 1, y ademas todo elemento de nR es mayor a x, ya que todo elemento de nR pertenece a r
--                                                                                                                      De 1. y 2. (NAVL _ l a nR) es un BST, por lo tanto balance t1@(NAVL _ l a nR) es un BST (Por Lema Auxiliar de la catedra)
--                                                                  {deleteAVL .3, caso a > x} 
--                                                                                                                     nR = deleteAVL x r -- Por HI1: nR es un BST
--                                                                                                                     (hL,hnR) = extractHeightDiference l ||| extractHeightDiference nR --> Se puede demostrar, pero no es necesario para el ejercicio
--                                                                                                                in balance t1@(NAVL (hR + hnL) l a nR)  --> Tenemos que demostrar que t1 es un BST, para eso tenemos que determinar que cumple con las invariantes de los BST
--                                                                                                                     1. Sabemos que r es un BST (por dato) y que (NAVL EAVL a r) es un BST (por dato)
--                                                                                                                     2. Sabemos que nR es un BST (por HI1) y que todo elemento de nR es mayor a x, ya que todo elemento de nR pertenece a r (propiedad)
--                                                                                                                     De 1. y 2.  (NAVL _ nL a r) es un BST, por lo tanto balance t1@(NAVL _ nL a r) es un BST (Por Lema Auxiliar de la catedra)
--                                                                 {deleteAVL .3, caso x > a}
--                                                                                                                     nL = deleteAVL x l -- Por HI2: nL es un BST
--                                                                                                                     (hR,hnL) = extractHeightDiference r ||| extractHeightDiference nL --> Se puede demostrar, pero no es necesario para el ejercicio
--                                                                                                                in balance t1@(NAVL (hR + hnL) nL a r)  --> Tenemos que demostrar que t1 es un BST, para eso tenemos que determinar que cumple con las invariantes de los BST
--                                                                                                                     1. Sabemos que l es un BST (por dato) y que (NAVL l a EAVL) es un BST (por dato)
--                                                                                                                     2. Sabemos que nL es un BST (por HI2) y que todo elemento de nL es menor a x, ya que todo elemento de nL pertenece a l (propiedad)
--                                                                                                                     De 1. y 2.  (NAVL _ nL a r) es un BST, por lo tanto balance t1@(NAVL _ nL a r) es un BST (Por Lema Auxiliar de la catedra)
-- De todas las formas posibles con un x generico. Podemos decir que deleteAVL x t@(NAVL h l a r) es un BST. 
-- FALTA DEMOSTRAR EL LEMA AUX 1: si t es un BST entonces deleteMax t es un BST. El cual la demostracion es facil y se usa la propiedad que se enuncia en los "2."

--___________________________________________________________________________________________________________________________________________________________________________
-- 06/8/2021 MESSU
--EJ 1: ZIPT, YA HECHO
--EJ 2: Promedios MAX
mergeA :: Ord a =>A.Arr a -> A.Arr a ->(a->a->Ordering)->A.Arr a
mergeA s1 s2 f = case showtA s1 of
                                                                      EMPTY -> s2
                                                                      ELT v -> let
                                                                                        (mayores,menores) =  filterA (\elem -> (f v elem) == GT) s2 ||| filterA(\elem -> (f v elem)/= GT) s2
                                                                                      in concatA mayores (concatA (singletonA v) menores)
                                                                      NODE l r -> let
                                                                                                   raiz = r ! 0
                                                                                                   (menoresraizenS2,mayoresraizenS2) = filterA (\elem -> (f elem raiz) /= GT) s2 ||| filterA(\elem -> (f elem raiz)== GT) s2
                                                                                                   (menores,mayores) = mergeA l menoresraizenS2 f ||| mergeA r mayoresraizenS2 f
                                                                                             in  mergeA menores mayores f

sortA :: Ord a => (a -> a -> Ordering) -> A.Arr a -> A.Arr a --S (|log n|)
sortA f s = case showtA s of
                                                       EMPTY -> emptyA
                                                       ELT v -> singletonA v
                                                       NODE l r -> let 
                                                                                    (nl,nr) = sortA f l ||| sortA f r
                                                                              in  mergeA nl nr f

promediosMax :: (A.Arr (A.Arr Int)) -> (Int, Int) --S(lg |p| + lg max |pi|)
promediosMax p | (lengthA p) == 0 = error "Promedio de lista vacia"
                                    | otherwise = let
                                                                      seqProm = mapA (\notasSi -> div (reduceA (+) 0 notasSi) (lengthA notasSi)) p --S(lg  (max|si|) )
                                                                      seqPromOrdenada = sortA compare seqProm --S (lg |p|)
                                                                 in if( lengthA seqPromOrdenada) == 1 then (seqPromOrdenada ! 0, seqPromOrdenada ! 0) else (seqPromOrdenada ! (lengthA seqPromOrdenada -1) , seqPromOrdenada ! (lengthA seqPromOrdenada -2) )  --S(1)
--Ej 3:
--a)  isMinHeap YA RESUELTO
--b) Tengo que demostrar que isLeaf y t = (elem y . leaves) t 
-- Recordemos el tipo de . :: (a -> b) ->(c -> a) -> c -> b
-- Sea t = N a [hijos], un minHeap, sabemos que cualquier subestructura de t tambien es un minHeap
-- Probemos la propiedad para t = E, el CASO BASE:
-- isLeaf y E = {isLeaf 1.} False = {elem 1.} elem y [] = {leaves 1.} elem y (leaves E) = {def .} (elem y . leaves) E
-- Se cumple la propiedad para el caso base
-- Probemos la propiedad para t = N a [hijos], asumo que la propiedad vale para toda la lista de los hijos de a
-- Por lo tanto. HI) isLeaf y h = (elem y . leaves) h, para todo h perteneciente a [hijos]
-- isLeaf y (N a [hijos]) = {isLeaf 3. Caso: null hijos AND x == y}
--                                                                                                                   True = {def. OR} (a == y) OR elem y  [] = {elem 2. "[]" } elem y [a] = {def leaves 2. "[a]" } elem y leaves (N a []) = {def .} (elem y . leaves) (N a []) ={por definicion del caso este, [hijos] = [}} (elem y . leaves) (N a [hijos]) = {def t} (elem y . leaves) t
--                                              {isLeaf 3. Caso: True}
--                                                                                                                   or (map (isLeaf y) [hijos]) = {HI, y def de map} or(map (elem y . leaves) [hijos]) = {def map} or(map (elem y) (map leaves [hijos])) = {Lema Catedra} (elem y . concat) (map leaves [hijos]) = {def . } (elem y) (concat (map leaves [hijos])) = {def leaves 2.} (elem y) (leaves (N a [hijos])) = {def .} (elem y . leaves) (N a [hijos]) = {def t} (elem y . leaves) t
-- De ambas formas se cumple la propiedad. Es VALIDA 
--___________________________________________________________________________________________________________________________________________________________________________
-- 30/04 Daria
--Ejercicio 1, heap para nosotros es minHeap
--a)
--Especificacion algebraica de delMin
--delMin empty = empty
--delMin (insert V empty) = empty
--delMin  (insert V (insert V' heap)) = (insert V' heap)

--take 0 _ = empty
--take n empty = empty
--take n (insert V empty) = insert V empty
--take n (insert V (insert V' heap)) = insert V (take n-1 (insert V' heap))

--b)
--data Heap a  = EH | NH a [Heap a]

minHijo::Ord a => [Heap a] -> (a, [Heap a])
minHijo [(NG a nietos)] = (a,nietos) --lista de un solo hijo
minHijo ((NG a nietos) : hermanos ) = let
                                                                                   (minValue, hijosMin) = minHijo hermanos
                                                                             in if minValue > a then (a, nietos) else (minValue,hijosMin) 

delMin ::Ord a => Heap a -> Heap a
delMin EG = EG
delMin (NG a []) = EG
delMin (NG a [EG]) = EG
delMin (NG a hijos) = let
                                                  (valMin,hijosMin) = minHijo hijos
                                            in NG valMin (hijosMin ++ hijos)

--Ejercicio 2
type FechaTupla = (Integer,Integer,Integer)
type Info = A.Arr (FechaTupla, Float,Float,Float)

--a)
extractFecha :: (Float, FechaTupla) -> FechaTupla
extractFecha (_,f) =f

funcMax :: (Float, FechaTupla) -> (Float,FechaTupla) -> (Float, FechaTupla)
funcMax (t1,f1) (t2,f2) = if t1 > t2 then (t1,f1) else (t2,f2)

fechaMaxDif :: Info -> FechaTupla --S(log |s|)
fechaMaxDif s | (lengthA s) == 0 = error "Lista Vacia!!!"
                              | otherwise =  let
                                                                 seqDiff = mapA (\(f,mini,maxi,_) -> (maxi - mini, f)) s
                                                                 seqRed = reduceA funcMax  (-1, (0,0,0)) seqDiff
                                                            in extractFecha seqRed
--b)
lluviaAcum :: Info -> Int -> Float -> FechaTupla --S(lg |s|)
lluviaAcum s a x | (lengthA s) == 0 = (toInteger a,0,0)
                                  | otherwise = let
                                                                    seqAño = filterA (\(f,_,_,_) -> ((\(y,m,d)-> y == (toInteger a)) f)) s --S(lg|s|)
                                                                    (acumSeq,acumTot) = scanA (\(f1,_,_,p1) (f2,mini,maxi,p2)-> (f2, mini,maxi,p1+p2)) ((toInteger a,0,0),0,0,0) seqAño --S(lg |s|)
                                                                    seqsinBase = concatA (dropA acumSeq 1) (singletonA acumTot) -- S(1)
                                                                    filterPreci = filterA (\(f,_,_,p) -> p >= x) seqsinBase --S(lg |s|)
                                                               in if(lengthA seqAño == 0 || lengthA filterPreci == 0 ) 
                                                                                                                   then (toInteger a,0,0) 
                                                                                                                   else (\(f,_,_,_) -> f) (filterPreci ! 0)

testInfoFechas :: Info --No te olvides del casting y de decir el tipo de los test que pones
testInfoFechas = fromListA[((1980,1,31),20,30,0),((1980,2,1),15,30,2),((1980,2,2),20,20,0)]
--___________________________________________________________________________________________________________________________________________________________________________
-- 30/04 
--1)
--a)
-- min es funcion Parcial
-- min (insert v empty) = v
-- min (insert v heap) = v
-- meld empty empty = empty
-- meld empty (insert v heap) = insert v heap
-- meld (insert v heap) empty = insert v heap
-- meld (insert v heap) (insert v' heap') = if v  > v' then insert v' (meld (insert v heap) heap')
--                                                                                                else insert v (meld heap (insert v' heap'))

--b)
minH :: Heap a -> a
minH (EG) = error "Heap Vacio"
minH (NG a _) = a

meld :: Ord a => Heap a -> Heap a -> Heap a
meld EG EG = EG
meld EG h2 = h2
meld h1 EG = h1
meld h1@(NG a1 hijos1) h2@(NG a2 hijos2) = if a1 > a2 then (NG a2 (h1:hijos2)) else (NG a1 (h2: hijos1)) 

--Ej 2
index :: Tree (Tree Char)-> Tree(Int,Int,Tree Char) --S(h)
index E = E
index t = indexaux t 0 

indexaux :: Tree (Tree Char) ->Int ->Tree (Int,Int, Tree Char) --S(h)
indexaux E  carryLeaf = E
indexaux (L a) carryLeaf = L (carryLeaf+1, size a, a) --Puedo usar cantLeaf a en vez de size a. Pero cambian los costos pasa a ser S( h + max hi) con max hi como la maxima altura de las palabras que se tienen en las hojas 
indexaux (N s l r) carryLeaf = let
                                                                  cantL = cantLeaf l --S(h)
                                                                  (nL,nR) = (indexaux l carryLeaf) ||| (indexaux  r (carryLeaf + cantL)) --S(h)
                                                                              in (g nL nR) --S(1)
                                                                        where g l r| (size l)==0 && (size r)==0 = E
                                                                                              | (size l) == 0 = r
                                                                                              | (size r) == 0 = l
                                                                                              | otherwise = N (size l + size r) l r

testIndesx :: Tree(Tree Char)
testIndesx = N 2 (L (N 7 (N 3 (L 'h') (L 'o')) (N 3 (L 'l') (L 'a')) )  )   ( L (N 7 (N 3 (L 'm') (L 'a')) (N 3 (L 'm') (L 'a'))) )
--___________________________________________________________________________________________________________________________________________________________________________
-- 18/12/2020
incluidos :: A.Arr (Float,Float) -> Int --S(lg |s|)
incluidos s | lengthA s == 0 = 0
                       | otherwise = let
                                                         seqCantIncluidos = tabulateA (\index->lengthA (filterA ((\(x,y) (z,w)-> if (x == y && z == x && w ==y) then True else if (x == y) then (x>z && w>y ) else ((x >= z &&  (z+w)>= (x+y)))) (s ! index))(takeA s index) ))(lengthA s) --S(tabulateA) = max S(filterA f (takeA s n)) = S (filterA f |s-1|) = lg|s-1| + max S(f si) = lg |s-1| + 1 = lg |s| 
                                                    in reduceA (+) 0 seqCantIncluidos --S(lg |s|)
                                                         
incluidosSeq :: A.Arr (Float, Float)
incluidosSeq = fromListA [(1,3),(1,2),(2,3),(2,2),(3,2)]
--___________________________________________________________________________________________________________________________________________________________________________
-- 18/12/2020
data BSTree a = EBST | NBST Int (BSTree a) a (BSTree a) deriving Show

maxBst :: Ord a => BSTree a  -> a --Funcion Parcial
maxBst (NBST h EBST x EBST) = x
maxBst (NBST h l x EBST) = x
maxBst (NBST h l x r) = maxBst r

delNeg :: BSTree Int -> BSTree Int
delNeg EBST = EBST
delNeg n@(NBST h EBST x r) = if  0 > x then (NBST h EBST 0 r) else n
delNeg n =  delNegaux n 0 --carry del anterior de la secuencia

delNegaux :: BSTree Int -> Int ->BSTree Int --S(2h) que es S(h)
delNegaux EBST anteriorNum = EBST
delNegaux n@(NBST h EBST x r) anteriorNum = if  0 > x then (NBST h EBST anteriorNum (delNegaux r x)) else (NBST h EBST x (delNegaux r x))
delNegaux n@(NBST h l x r) anteriorNum = let
                                                                                              anteriorNumNuevo = maxBst l --S(h)
                                                                                        in if 0 > x && 0 > anteriorNumNuevo then let
                                                                                                                                                                                   (nL,nR) = delNegaux l anteriorNum ||| delNegaux r anteriorNum --S(h)
                                                                                                                                                                              in (NBST h nL anteriorNum nR)
                                                                                             else if 0 > x && anteriorNumNuevo >= 0 then let
                                                                                                                                                                                               (nL,nR) = delNegaux l anteriorNum ||| delNegaux r anteriorNumNuevo
                                                                                                                                                                               in (NBST h nL anteriorNumNuevo nR)
                                                                                                       else let
                                                                                                                      (nL,nR) = delNegaux l anteriorNum ||| delNegaux r x
                                                                                                                 in (NBST h nL x nR)

inordenBST :: BSTree a -> [a]
inordenBST EBST = []
inordenBST (NBST _ EBST a EBST) = [a]
inordenBST (NBST s l x r) = inordenBST l ++ [x] ++ inordenBST r

--Pachi
auxScan :: Int -> Int -> Int
auxScan n 0 = n
auxScan n m = n + m
posUnos :: A.Arr Int
posUnos = fromListA [1,1,1,1,1,1,1,1]
--if cantCeros>0 scanA auxScan (cantCeros -1) posUnos, saco base y uno
--else cantCeros == 0 scanA auxScan cantCeros posUnos, mequedo solo con la seq del scan

negSeq :: BSTree Int
negSeq = NBST 8 (NBST 3 (NBST 1 EBST (-1) EBST ) (-2) (NBST 1 EBST (-2) EBST) ) 3 (NBST 4 (NBST 2 EBST 4 (NBST 1 EBST (-5) EBST)) 1 (NBST 1 EBST (-1) EBST))