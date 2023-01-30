module Ficha1 where
import Data.Char
--1.

perimetro :: Double -> Double
perimetro a = 3.14 * 2 * a


dist :: Float -> Float -> Float -> Float -> Float
dist a b c d = sqrt ( (c - a)^2 + (d - b)^2 )


primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)


multiplo :: Int -> Int -> Bool
multiplo a b = if mod b a == 0 then True else False


--truncaImpar	:: [a] -> [a]
--truncaImpar a = if mod a 2 == 0 then a else tail a

truncaImpar :: [a] -> [a]
truncaImpar a = if length a `mod` 2 == 0 then a else tail a 

max2 :: Int -> Int -> Int
max2 a b = if a > b then a else b


max3 :: Int -> Int -> Int -> Int
max3 a b c = max2 (max2 a b) c


-- 2. a)

nRaizes :: Float -> Float -> Float -> Int
nRaizes a b c = if determinante > 0  
    then 2 
        else if determinante == 0 then 1 
            else 0
                where determinante = (b^2 - 4 * a * c)

-- b)
raizes a b c 
    | n == 2 = [x1, x2] 
    | n == 1 = [x1] 
    | n == 0 = [] 
    where n = nRaizes a b c
          determinante = b^2 - 4*a*c
          (x1,x2) = (((-b) + sqrt (determinante))/ (2*a), ((-b) - sqrt (determinante))/ (2*a))

-- 3)
-- a)

type Hora = (Int,Int)

horavalida :: Hora -> Bool
horavalida (h,m) = if (h >= 0 && h <= 23 && m >= 0 && m<= 59) then True
                   else False


-- b)

hora1 :: Hora -> Hora -> Hora
hora1 (h1,m1) (h2,m2) = if (horavalida (h1,m1) && horavalida (h2,m2))
                           then 
                                if (h1 > h2 || (h1 == h2 && m1 > m2))
                                  then (h1,m1)
                                else (h2,m2)
                        else error "HORA INVÁLIDA!"

-- c)

horapmin :: Hora -> Int
horapmin (h1,m1) = if (horavalida (h1,m1)) 
                      then h1 * 60 + m1
                   else error "HORA INVÁLIDA!"

-- d)

hora3 :: Int -> Hora
hora3 x = (x `div` 60 , x `mod` 60)

-- e)

difhora :: Hora -> Hora -> Int
difhora (h1,m1) (h2,m2) = if (horavalida (h1,m1) && horavalida (h2,m2))
                            then (h1 * 60 + m1) - (h2 * 60 + m2)
                        else error "HORA INVÁLIDA!"

-- f)
{-
hora5 :: Int -> Hora -> Hora
hora5 x (h1,m1) = if (horavalida (h1,m1) 
                    then x + m1
                      if (m1 + x) >= 60 
    	                 then (h1 + (m1 + x) `mod` 60 , (m1 + x) - 60) 
    	              else (h1, m1 + x)
                  else error "HORA INVÁLIDA!"

hora5 :: Int -> Hora -> Hora
hora5 x (h1,m1) = if horavalida (h1,m1) 
                      then x + m1 if (x + m1) >= 60
                                    then (h1 + (x + m1 `mod` 60) , x + m1 - 60)
                                  else (h1,x + m1)
-}

-- 4.

-- a)

data Horaa = H Int Int deriving (Show, Eq)

horavalida' :: Horaa -> Bool
horavalida' (H h m) = h `elem` [0..23] && m `elem` [0..59]

-- 5.

data Semaforo = Verde | Amarelo | Vermelho deriving (Show,Eq)

-- a) 
next :: Semaforo -> Semaforo
next s = case s of Vermelho -> Verde
                   Verde -> Amarelo
                   Amarelo -> Vermelho

-- b)
stop :: Semaforo -> Bool
stop s = case s of Vermelho -> True
                   Verde -> False
                   Amarelo -> True

-- c)
safe :: Semaforo -> Semaforo -> Bool
safe s1 s2 = s1 == Vermelho || s2 == Vermelho

-- 6.

{-
data Ponto = Cartesiano Double Double | Polar Double Double deriving (Show,Eq)

-- a)
posx :: Ponto -> Double
posx x y = case x y of Cartesiano -> if x x >= 0 then x else (-x)
                       Polar -> if y == pi / 2 then 0 else x * cos y
-} 

data Ponto = Cartesiano Double Double | Polar {distPonto :: Double, anguloPonto :: Double} deriving (Show,Eq)

-- a)
posx :: Ponto -> Double
posx ponto = case ponto of Cartesiano x _ -> x
                           Polar d a -> if a == pi/2 then 0 else d * cos a

-- b)
posy :: Ponto -> Double
posy ponto = case ponto of Cartesiano _ y -> y
                           Polar d a -> if a == pi then 0 else d * sin a

-- c)
raio :: Ponto -> Double
raio ponto = case ponto of Cartesiano x y -> sqrt $ x^2 + y^2
                           Polar d _ -> d

-- d)
angulo :: Ponto -> Double
angulo ponto = case ponto of Cartesiano x y -> if x < 0 && y == 0 then pi else
                                               if x < 0 then pi + atan (y/x) else
                                               atan (y/x)
                             Polar _ a -> a

-- e)
distancia :: Ponto -> Ponto -> Double
distancia ponto1 ponto2 = sqrt (((posx ponto1 - posx ponto2) ^ 2) + (posy ponto1 - posy ponto2) ^ 2)

-- 8.
-- a)
isLower' :: Char -> Bool
isLower' x = x `elem` ['a'..'z']

-- b)
isDigit' :: Char -> Bool
isDigit' x = x `elem` ['0'..'9']

-- c)
isAlpha' :: Char -> Bool
isAlpha' x = if isLower' x then True else x `elem` ['A'..'Z']

-- d)
toUpper' :: Char -> Char
toUpper' ch = if isLower' ch then chr (ord ch - 32) else ch

-- e)
inToDigit' :: Int -> Char
inToDigit' n = chr (n + 48)

-- f)
digitToInt' :: Char -> Int
digitToInt' ch = ord ch - 48