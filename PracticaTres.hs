module PracticaTres where

import Data.List

--1 Practica 3
type Color = (Float, Float, Float)

mezclar :: Color -> Color -> Color
mezclar (r, g, b) (rr, gg, bb) = ( (r+rr)/2, (g+gg)/2,(b+bb)/2)

--2 Practica 3
type Linea = ([Char], Int)

vacia :: Linea
vacia = ([],0)

moverIzq :: Linea -> Linea
moverIzq (xs, 0) = (xs, 0)
moverIzq (xs,n) = (xs, n-1)

moverDer :: Linea -> Linea
moverDer (xs, n) = if length xs > n then (xs, n+1) else (xs, n)

moverIni :: Linea -> Linea
moverIni (xs, _) = (xs, 0)

moverFin :: Linea -> Linea
moverFin (xs, _) = (xs, length xs)

insertar :: Char -> Linea -> Linea
insertar c (xs,n) = let
                                           fh  = take n xs
                                           sh = drop n xs
                                    in (fh ++ [c] ++ sh, n+1)

borrar:: Linea -> Linea
borrar (xs, 0) = (xs, 0)
borrar (xs, n) = let
                                    fh = take (n-1) xs
                                    sh = drop n xs
                               in (fh ++ sh, n-1)

--3 Practica 3
data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving Show
--a)
headCL :: CList a -> a
headCL (CUnit a) =  a
headCL (Consnoc l _ _) = l

agregarFin :: CList a -> a -> CList a
agregarFin EmptyCL dato          = CUnit dato
agregarFin (CUnit a) dato        = Consnoc a EmptyCL dato
agregarFin (Consnoc l xs r) dato = Consnoc l ( agregarFin xs r) dato

agregarInicio:: a -> CList a -> CList a
agregarInicio dato EmptyCL          = CUnit dato
agregarInicio dato (CUnit a)        = Consnoc dato EmptyCL a
agregarInicio dato (Consnoc l xs r) = Consnoc dato ( agregarInicio l xs) r

tailCL :: CList a -> CList a
tailCL (CUnit a) = EmptyCL
tailCL (Consnoc _ xs r) = agregarFin xs r

isEmptyCL :: CList a -> Bool
isEmptyCL EmptyCL = True
isEmptyCL _ = False

isCUnit :: CList a -> Bool
isCUnit (CUnit a) = True
isCUnit _ = False
--b)
reverseCL :: CList a -> CList a
reverseCL xs = case xs of
                  EmptyCL          -> EmptyCL
                  (CUnit a)        -> (CUnit a)
                  (Consnoc l ys r) -> (Consnoc r (reverseCL ys) l)
--c)
aux :: CList a-> CList a-> [CList a]
aux (CUnit a) carry = [agregarFin carry a]
aux (Consnoc a l z) carry = let
                                                           newCarry = agregarFin carry a
                                                      in [newCarry] ++ (aux (agregarFin l z) newCarry) 

initCList :: CList a -> [CList a] --Puedo resolverlo de esta forma porque agregar inicio y final tienen el mismo peso... sino priorizo el agregar al inicio como en listas [a]
initCList (EmptyCL) = [EmptyCL]
initCList (CUnit a) = [CUnit a]
initCList (Consnoc a l z) = EmptyCL : (CUnit a) : (aux (agregarFin l z) (CUnit a) )

listToCL :: [a] -> CList (a)
listToCL []     = EmptyCL
listToCL (x:xs) = agregarInicio x (listToCL xs) 
--{ OTRA VERSION
--initsCL :: CList a -> CList (CList a)
--initsCL cl = listToCL (initCList cl)

--initaux :: CList a -> [CList a]
--initaux EmptyCL = [EmptyCL]
--initaux lista   = EmptyCL : (map (consCL (headCL lista)) (initaux (tailCL lista)))

--listToCL :: [a] -> CList (a)
--listToCL []     = EmptyCL
--listToCL (x:xs) = consCL x (listToCL xs) 

--initsCL :: CList a -> CList (CList a)
--initsCL lista = listToCL (initaux lista)
--}

--d)
lastsCL :: CList a -> CList (CList a)
lastsCL lista = listToCL (map reverseCL (initCList (reverseCL lista)))
--e) Hecho en la carpeta

--Ejercicio 4 en la carpeta

--Ejercicio 5 
data Tree a  = Empty | Node (Tree a) a (Tree a) deriving Show

--b
balanceado :: a -> Int -> Tree a
balanceado a 0 = Empty
balanceado a 1 = Node Empty a Empty
balanceado a n = let
                                        ri = balanceado a  (div n 2)
                                        rr = balanceado a  (div (n-1) 2)
                                   in Node ri a rr

--a
completo :: a -> Int -> Tree a
completo a 0  = Node Empty a Empty
completo a n = Node (completo a (n-1)) a (completo a (n-1))

--Ejercicio 6
data GenTree a = EmptyG | NodeG a [GenTree a] deriving Show
data BinTree a = EmptyB | NodeB (BinTree a) a (BinTree a) deriving Show

auxBin :: [GenTree a] -> BinTree a  --AuxBin se encarga de procesar a los hermanos de los nodos poniendolos en la rama derecha, mientras que si estos tienen  hijos llamo de nuevo a g2bt para convertirlos y a la izq
auxBin [EmptyG] = EmptyB
auxBin [] = EmptyB
auxBin ((NodeG a hijos):hermanos) = NodeB (auxBin hijos) a (auxBin hermanos)

g2bt :: GenTree a -> BinTree a
g2bt EmptyG = EmptyB
g2bt (NodeG a [EmptyG]) = NodeB EmptyB a EmptyB
g2bt (NodeG a hijos) = NodeB (auxBin hijos) a EmptyB

arbolPractica = NodeG 'A' [NodeG 'B' [NodeG 'G' [NodeG 'M' [EmptyG], NodeG 'N' [EmptyG]], NodeG 'H' [EmptyG], NodeG 'I' [EmptyG] ], NodeG 'C' [EmptyG], NodeG 'D' [EmptyG], NodeG 'E' [NodeG 'J' [NodeG 'O' [EmptyG]],NodeG 'K' [NodeG 'P' [EmptyG]]], NodeG 'F' [EmptyG], NodeG 'G'[NodeG 'L' [EmptyG]] ]

auxGen :: BinTree a -> [GenTree a]
auxGen EmptyB = [EmptyG]
auxGen (NodeB hijos a hermanos) = [NodeG a (auxGen hijos)] ++ auxGen hermanos

bt2g :: BinTree a -> GenTree a
bt2g EmptyB = EmptyG
bt2g (NodeB hijos a _) = NodeG a (auxGen hijos)

--Ejercicio 7 en carpeta
--Ejercicio 8
member :: Ord a => BinTree a -> a -> Bool
member EmptyB z  = False
member (NodeB i a d) z = auxMember (NodeB i a d) z a

auxMember :: Ord a => BinTree a -> a -> a -> Bool
auxMember EmptyB z carry = z == carry
auxMember (NodeB i a d) z carry = if z > a 
                                                                       then (auxMember d z carry)
                                                                       else (auxMember i z a)

testMember = member (NodeB (NodeB (NodeB EmptyB 7 EmptyB) 8 (NodeB EmptyB 9 EmptyB)) 10 (NodeB (NodeB EmptyB 11 EmptyB) 12 (NodeB EmptyB 13 EmptyB))) 13