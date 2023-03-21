module PracticaTres where

import Data.List 

data Tree a = Empty | Leaf a | Join (Tree a) (Tree a) deriving Show

--Ejercicio 3
sufijos :: Tree Int  -> Tree (Tree Int)
sufijos t = fst (sufijoaux t t)

sufijoaux :: Tree Int -> Tree Int -> (Tree (Tree Int), Tree Int)
sufijoaux Empty t = (Empty, Empty)
sufijoaux (Leaf _) t = let
                                               resEliminacion = deletePrimero t
                                          in (Leaf resEliminacion, resEliminacion)
sufijoaux (Join l r) t =  let
                                               (nl, resEliminacionL) = sufijoaux l t
                                               (nr, resEliminacionR) = sufijoaux r resEliminacionL
                                            in (Join nl nr, resEliminacionR)   

deletePrimero :: Tree Int -> Tree Int
deletePrimero Empty = Empty
deletePrimero (Join Empty r) = deletePrimero r
deletePrimero (Leaf a) = Empty
deletePrimero (Join (Leaf _) (Leaf r)) = Leaf r
deletePrimero (Join l r) = Join (deletePrimero l) r 

tabulateT :: (Int -> a) -> Int -> Tree a
tabulateT f 0 = Empty
tabulateT f n = tabulateaux f n 0

tabulateaux ::(Int -> a) -> Int -> Int -> Tree a
tabulateaux f n carry | (mod n 2) \= 0 = let
                                                                                     raiz = (div n 2) +1 + carry
                                                                                     l = tabulateaux f (raiz -1-carry) carry
                                                                                     r = tabulateaux f (raiz -1-carry) raiz
                                                                                in if (size l == 0 && size r == 0) then (L (f raiz)) else (N n l r)
                                           | (mod n 2) == 0 = let
                                                                                     raiz = (div n 2) +1 + carry
                                                                                     l = tabulateaux f (raiz -1-carry) carry
                                                                                     r = tabulateaux f (raiz -2-carry) raiz
                                                                                in if (size l == 0 && size r == 0) then (L (f raiz)) else (N n l r)

