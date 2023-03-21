module PracticaTres where

import Data.List


--Ejercicio 6
data GenTree a = NodeG a [GenTree a] deriving Show

arbolPractica = NodeG 'A' [NodeG 'B' [NodeG 'G' [NodeG 'M' [NodeG 'z' []], NodeG 'N' []], NodeG 'H' [], NodeG 'I' [] ], NodeG 'C' [], NodeG 'D' [], NodeG 'E' [NodeG 'J' [NodeG 'O' []],NodeG 'K' [NodeG 'P' []]], NodeG 'F' [], NodeG 'G'[NodeG 'L' []] ]
arbolPracticaInt = NodeG 11 [NodeG 2 [NodeG 5 [NodeG 1 [NodeG 1000 []], NodeG 121212 []], NodeG 444 [], NodeG 1212 [] ], NodeG 100000 [], NodeG 999 [], NodeG 213 [NodeG 4 [NodeG 23 []],NodeG 1513515 [NodeG 32 []]], NodeG 90 [], NodeG 2344 [NodeG 0 []] ]

alturaAGT :: GenTree a -> Int --Altura arbol general sin Empty
alturaAGT (NodeG a []) = 1
alturaAGT (NodeG a hijos) = 1 + foldr max 0 (map alturaAGT hijos)

data GenTreeE a = EmptyGE | NodeGE a [GenTreeE a] deriving Show

arbolPracticaE = NodeGE 'A' [NodeGE 'B' [NodeGE 'G' [NodeGE 'M' [EmptyGE], NodeGE 'N' [EmptyGE]], NodeGE 'H' [EmptyGE], NodeGE 'I' [EmptyGE] ], NodeGE 'C' [EmptyGE], NodeGE 'D' [EmptyGE], NodeGE 'E' [NodeGE 'J' [NodeGE 'O' [EmptyGE]],NodeGE 'K' [NodeGE 'P' [EmptyGE]]], NodeGE 'F' [EmptyGE], NodeGE 'G'[NodeGE 'L' [EmptyGE]] ]

alturaAGTE :: GenTreeE a -> Int --Altura arbol general con Empty
alturaAGTE (EmptyGE) = 0
alturaAGTE (NodeGE a hijos) = 1 + foldr max 0 (map alturaAGTE hijos)

mapGTree :: (a -> b) -> GenTreeE a -> GenTreeE b
mapGTree f EmptyGE = EmptyGE
mapGTree f (NodeGE a hijos) = NodeGE (f a) (map (mapGTree f) hijos)

maxAGT :: (Eq a, Ord a) => GenTree a -> a
maxAGT (NodeG a []) = a
maxAGT (NodeG a hijos) = foldr max a (map maxAGT hijos)