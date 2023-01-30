module Ficha2 where
import Data.Char (ord)
import Data.List (partition)

-- 2.

-- a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = 2 * h : dobros t

-- b)
num0corre :: Char -> String -> Int 
num0corre _ [] = 0
num0corre a (h:t) = if a == h then 1 + num0corre a t
                    else num0corre a t    

-- c) 
positivos :: [Int] -> Bool
positivos [] = False
positivos [n] = if n >= 0 then True else False
positivos (h:t) = if h >= 0 then positivos t
                  else False

-- d)
soPos :: [Int] -> [Int]
soPos [] = []
soPos (h:t) = if h >= 0 then h:soPos t
              else soPos t

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if h < 0 then h + somaNeg t
                else somaNeg t

-- f) 
tresUlt :: [a] -> [a] 
tresUlt [] = []
tresUlt [n] = [n]
tresUlt [m,n] = [m,n]
tresUlt [m,n,o] = [m,n,o]
tresUlt l = tresUlt l

tresUlt' :: [a] -> [a]
tresUlt' [] = []
tresUlt' (h:t) 
              | length (h:t) <= 3 = (h:t)
              | otherwise = tresUlt' t

-- g)
segundos :: [(a,b)] -> [b]
segundos [] = []
segundos ((x,xs):t) = xs : segundos t

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros _ [] = False
nosPrimeiros n ((x,xs):t) = if n == x then True 
                            else  nosPrimeiros n t

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [(x,y,z)] = (x,y,z)
sumTriplos ((x,y,z):(xs,ys,zs):t) = sumTriplos ((x+xs, y+ys, z+zs):t) 

-- 3.

-- a)
soDigitos :: [Char] -> [Char] 
soDigitos [] = []
soDigitos (h:t) = if h `elem` ['0'..'9'] then h:soDigitos t
                  else soDigitos t

-- b)
minusculas :: [Char] -> Int 
minusculas [] = 0
minusculas (h:t) = if h `elem` ['a'..'z'] then 1 + minusculas t
                   else minusculas t

-- c)
nums :: String -> [Int]
nums [] = []
nums (h:t) = if h `elem` ['0'..'9'] then (ord h - ord '0') : nums t 
             else nums t

-- 4.
type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)
conta :: Int -> Polinomio -> Int
conta _ [] = 0
conta n ((x,y):t) = if n == y then 1 + conta n t
                    else conta n t 

-- b)
grau :: Polinomio -> Int
grau [(x,y)] = y
grau ((x,y):t) = if y >= grau t then y else grau t

-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau _ [] = []
selgrau n ((x,y):t) = if n == y then ((x,y):selgrau n t)
                      else selgrau n t

-- d)
deriv :: Polinomio -> Polinomio 
deriv [] = []
deriv ((x,y):t) = (fromIntegral (y)*x,y-1) : deriv (t)

-- e)
calcula :: Float -> Polinomio -> Float
calcula _ [] = 0
calcula n ((x,y):t) = x * n ^ y + calcula n t

-- f)
simp :: Polinomio -> Polinomio
simp [] = []
simp ((x,y):t) = if x /= 0 then (x,y):simp t
                 else simp t 

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult _ [] = []
mult (a,b) ((x,y):t) = (a*x,y+b):mult (a,b) t

-- h)
{-
normaliza :: Polinomio -> Polinomio 
normaliza [] = []
normaliza ((x,y):t) =                               
-}
-- i)

-- j)
{-
produto :: Polinomio -> Polinomio -> Polinomio
produto _ [] = []
produto [] _ = []
produto ((a,b):c) ((x,y):t) 
-}

-- k)
