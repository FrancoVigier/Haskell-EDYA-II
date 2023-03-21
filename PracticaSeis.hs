module PracticaSeis where

import Data.List 

import qualified Arr as A
import Arr((!))

import Seq
import ArrSeq
import Par((|||))

--Ej 1
promedios ::  A.Arr Int -> A.Arr Float
promedios s = tabulateA (\y->(fromIntegral (reduceA (+) 0 (takeA s (y+1))) :: Float)/(fromIntegral (y+1) :: Float))   (lengthA s)

mayores :: A.Arr Int ->  Int
mayores s = reduceA (+) 0 (tabulateA f (lengthA s))
                         where f a = let
                                                       prefijo = takeA s (a + 1)
                                                       maximo = reduceA max 0 prefijo
                                                 in if(maximo == (s ! (a)) && a > 0 && (noRepeNum maximo prefijo) == 1) then 1 else 0


noRepe:: Int -> A.Arr Int ->Bool --Tambien anda pero en mayores siempre el maximo va a ser el ultimo en el prefijo por ende siempre va a estar
noRepe m s = reduceA (||) False (tabulateA (\y -> m==( s  ! (y)) )(lengthA s))

noRepeNum:: Int -> A.Arr Int -> Int --Cuenta la cantidad de recurrencias de m en s
noRepeNum m s = reduceA (+) 0 (tabulateA (\y -> if (m==(s ! (y))) then 1 else 0 )(lengthA s))

--Ej 2
--fibSeq :: Int->(A.Arr Int, Int) No sirve porque fibbo no es asociativa
--fibSeq n =  scanA fibbo 0 (tabulateA id (n))
--                     where fibbo a b = a + (fib (b)) 

--fib :: Int -> Int
--fib (-1) = 1
--fib 0 = 0
--fib 1 = 1
--fib n = let 
--                   (nl,nr) = fib (n-1) ||| fib (n-2)
--             in nl+nr

potmatriz ::(Int, Int, Int, Int) -> (Int, Int, Int, Int) ->(Int, Int, Int, Int) 
potmatriz (m11,m12,m21,m22) (g11,g12,g21,g22) = let
                                                                                                              f11 = m11 * g11 + m12 * g21
                                                                                                              f12 = m11*g12 + m12*g22
                                                                                                              f21 = m21*g11 + m22 *g21
                                                                                                              f22 = m21 * g12 + m22 * g22
                                                                                                        in (f11,f12,f21,f22)

fibSeq :: Int-> A.Arr Int
fibSeq 0 = fromListA[0]
fibSeq 1 = fromListA [0,1]
fibSeq n = let 
                          (prefijosSeq, ultimoValor) = scanA potmatriz (1,1,1,0) (tabulateA (\y->(1,1,1,0)) n)
                     in  concatA (singletonA ((\(_,_,_,f0)->f0) (prefijosSeq ! 0)))  (mapA (\(_,fn,_,_)->fn) prefijosSeq)

-- concatA (singletonA ((\(_,_,_,f0)->f0) (prefijosSeq ! 0))) (concatA (mapA (\(_,fn,_,_)->fn) prefijosSeq) (singletonA ((\(_,fn,_,_)->fn) ultimoValor)))


--Ej 3
agua :: Int -> Int -> Int -> Int
agua ml mr bi = let
                                     a = (min ml mr) - bi
                                in if (0 >= a) then 0 else a

aguaHist :: A.Arr Int -> Int
aguaHist s = reduceA (+) 0 (tabulateA g (lengthA s))
                         where g i = let
                                                           l = takeA s i
                                                           r = dropA s i
                                                           maxl = reduceA max 0 l
                                                           maxr = reduceA max 0 r
                                                           a = agua maxl maxr (s ! i)
                                                     in a 

histo = fromListA [2,3,4,7,5,2,3,2,6,4,3,5,2,1]

--Ej 4
data Paren = Close | Open deriving Show

--a
matchP :: A.Arr Paren -> Bool --SINO TENGO QUE CONTROLAR QUE CADA PREFIJO NO CONTIENE MENOS OPEN QUE CLOSE, SOLO USO EL REDUCE EN LUGAR DEL SCAN  Y LO DEVUELVO. PERO SI TENGO QUE CONTROLARLO USO EL SCAN
matchP s = let 
                            (controlarPrefijosOC, cantidadNetaTotal) = scanA sumarTupla (0,0) (mapA ponerTupla s) -- a cada prefijo le saco la cantidad neta de Open y Close. Si hay mas Close que Open (cosa que se tiene que invalidar) el valor para ese prefijo es (-1,0). Y el valor neto de la reduccion esta en el ultimoValor
                       in (cantidadNetaTotal == (0,0) && (reduceA (||) False (tabulateA (\index -> (controlarPrefijosOC ! index) == (-1,0)) (lengthA controlarPrefijosOC) )) == False ) --Para que sea verdad, el ultimo valor tiene que ser (0,0) y ninguna reduccion de los prefijos tiene que dar (-1,0)

ponerTupla:: Paren -> (Int, Int)
ponerTupla Open = (1,0)
ponerTupla Close = (0,1)

sumarTupla :: (Int, Int) -> (Int,Int) -> (Int,Int)
sumarTupla (o1,c1) (o2,c2) = let
                                                                on = o1 + o2
                                                                cn = c1 + c2
                                                          in (on - cn, 0)
-- if (cn > on) then error "Mas Close que Abiertos en un Prefijo" else  (on - cn, 0) No pongo esto porque ya de base cada prefijo no tiene mas Closes que Open. haciendo que toda la secuencia solo haya mas Open que Closes

--Ej 5

mcss :: A.Arr Int -> Int
mcss s = (\(s,_,_,_) -> s) (reduceA combine (0,0,0,0) (mapA base s))

combine:: (Int, Int, Int, Int) -> (Int, Int, Int, Int) ->(Int, Int, Int, Int) 
combine(m,p,s,t) (m2,p2,s2,t2) = (max (s + p2) (max m m2), max p (t + p2), max s2 (s + t2), t + t2)

base:: Int  -> (Int, Int, Int, Int)
base v = let
                        nv = max v 0
                 in (nv,nv,nv, v)

sccml:: A.Arr Int  -> Int
sccml s = mcss (pasardeSeqaSeqdeCrecimiento s) -- El (-lengthA s) entre secuencias de crecimiento hace que si o si el mcss te de la secuencia de 1's mas larga, por lo tanto el crecimiento contiguo maximo

pasardeSeqaSeqdeCrecimiento :: A.Arr Int -> A.Arr Int --Dada una secuencia s, crea otra donde pone 1 si Xi es menor a Xi+1 (crecimiento)y pone (-length s) de caso contrario. Por lo tanto un  crecimiento de N va a estar representado por N 1´s consecutivos, y entre cada crecimiento va a haber un -length s que los separe
pasardeSeqaSeqdeCrecimiento s = tabulateA(\index -> if (index == ((lengthA s) -1)  ) then (-(lengthA s)) else if ((s ! (index +1)) > (s ! index)) then 1 else (-(lengthA s)) ) (lengthA s)


--Ej 6
multiplos :: A.Arr Int ->  Int
multiplos s = reduceA (+) 0 (tabulateA (\index-> lengthA (f (s ! index) (dropA s (index+1)))) (lengthA s))
                         where f dato seq = filterA (\sj -> (mod dato sj) == 0) seq

--Ej 7
esta:: Int -> A.Arr Int -> Bool
esta x s = (lengthA (filterA (\dato ->  dato == x) s)) /= 0

unique :: A.Arr Int -> A.Arr Int
unique s = mapA(\(x,y)-> x)(filterA (\(x,y)->y == False)(tabulateA (\index ->  ((s ! index), (esta (s!index) (takeA s index))) ) (lengthA s)))

solounicos :: A.Arr Int -> A.Arr Int
solounicos s = mapA(\(x,y)-> x)(filterA (\(x,y)->y == False)(tabulateA (\index ->  ((s ! index), (esta (s!index) (takeA s index)) || (esta (s!index) (dropA s (index+1)))) ) (lengthA s)))