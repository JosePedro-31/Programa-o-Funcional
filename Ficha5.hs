module Ficha5 where

import Data.List
import Data.Char

-- 1.

--a)
any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (h:t) = f h || any' f t

--b)
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

--c)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (h:t) = if f h then h : takeWhile' f t
                    else []

--d)
dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (h:t) = if f h then dropWhile' f t
                    else t

--e)
span' :: (a-> Bool) -> [a] -> ([a],[a])
span' _ [] = ([],[])
span' f (h:t) | f h = (h:s1,s2)
              | otherwise = ([],h:t)
    where (s1,s2) = span' f t

--f)
deleteBy' :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy' f x (h:t) = if f x h then t
                    else h : deleteBy' f x t

--g)
sortOn' :: Ord b => (a -> b) -> [a] -> [a]
sortOn' f [] = []
sortOn' f [x] = [x]
sortOn' f (h:s:t) = if f h < f s then h : sortOn' f (s:t)
                    else s : sortOn' f (h:t)

-- 2.

type Polinomio = [Monomio]
type Monomio = (Float, Int)

-- a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau n [] = []
selgrau n l = filter (\x -> n == snd(x)) l

-- b)
conta :: Int -> Polinomio -> Int
conta n l = length (filter (\x -> n == snd(x)) l)


-- c)
grau :: Polinomio -> Int
grau l = foldl (\maior x -> if maior > snd(x) then maior else snd(x)) 0 l


-- e)
calcula :: Float -> Polinomio -> Float 
calcula n l = foldl (\acc (x,y) -> acc + x * (n^y)) 0 l


-- f)
simp :: Polinomio -> Polinomio 
simp l = filter (\x -> fst x /= 0) l


-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult (a,b) l = map (\n -> (fst n * a, snd n + b)) l -- os graus não se multiplicam, somam-se


-- h)
ordena :: Polinomio -> Polinomio
ordena l = sortOn (\x -> snd x) l


-- i)
normaliza :: Polinomio -> Polinomio
normaliza (h:t) = foldl (\((num,grau):t) (n,g) -> if grau == g then ((num+n),grau):t 
                                              else (n,g) : (num,grau) : t) [h] t


-- j)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza $ (++) p1 p2


-- 3.

type Mat a = [[a]]

{- 
[1,2,3]
[0,4,5]
[0,0,6]

é representada por:

[[1,2,3], [0,4,5], [0,0,6]]
-}


-- a)
dimOK :: Mat a -> Bool
dimOK [x] = True 
dimOK (l1:l2:t) | length l1 == length l2 = dimOK (l2:t)
                | otherwise = False

-- b)
dimMat :: Mat a -> (Int,Int)
dimMat [] = (0,0)
dimMat (h:t) = (length (h:t), length h)


-- c)
addMat :: Num a => Mat a -> Mat a -> Mat a
addMat [] [] = []
addMat [] [m2] = [m2]
addMat [m1] [] = [m1]
addMat (h:t) (hx:tx) = addAux h hx : addMat t tx


addAux :: Num a => [a] -> [a] -> [a]
addAux [] [] = []
addAux [] l2 = l2
addAux l1 [] = l1
addAux (x:xs) (y:ys) = (x+y) : addAux xs ys


-- f)