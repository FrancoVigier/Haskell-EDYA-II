module PracticaDos where

import Data.List

--13 Practica 2
noEsta :: Int -> [Int] -> Bool
noEsta x [] = True
noEsta y (x:xs) =  not (y == x) && (noEsta y xs)

unique :: [Int] -> [Int]
--unique xs = [x | x <- xs, noEsta x xs] No funciona porque no se avanza en la lista xs, si asi pasara si funcionaria
unique xs = [x | (x,i) <- zip xs [1 ..], not (elem x (take (i-1) xs))]

--14 Practica 2
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct  xs ys = sum [ x * y | (x , y) <- zip xs ys]