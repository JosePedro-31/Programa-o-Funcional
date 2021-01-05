module CinquentaQ where

import Data.List

-- 1)
enumFrontTo' :: Int -> Int -> [Int]
enumFrontTo' x y  
                  | x > y = []
                  | x == y = [x]
                  | otherwise = x : enumFrontTo' (x+1) y

-- 2)
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z 
                     | x > z = []
                     | x == z = [x]
                     | otherwise = x : enumFromThenTo' y (2 * y - x) z


numFromThenTo :: Int -> Int -> Int -> [Int]
numFromThenTo a b c | a>=c = []
                    | otherwise = a : numFromThenTo (a+x) (b+x) c
                    where x = b-a

-- 3)
concatenar :: [a] -> [a] -> [a]
concatenar [] l = l 
concatenar (h:t) l = h : concatenar (t) l 

teste :: [a] -> [a] -> [a]
teste [] [] = []
teste l [] = l
teste [] l = l 
teste (h:t) l = h : teste (t) l

-- 4)
calcula :: [a] -> Int -> a
calcula [] x = error "Lista Vazia"
calcula (h:t) x = if x > length(h:t) then error "Impossível"
                  else if x == 0 then h
                       else calcula t  (x-1)

-- 5)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h:t) = reverse' (t) ++ [h]

-- 6)
take' :: Int -> [a] -> [a]
take' n [] = []
take' 0 (h:t) = []
take' n (h:t) = h : take' (n - 1) t

-- 7)
drop' :: Int -> [a] -> [a]
drop' n []    = []
drop' 0 (h:t) = (h:t)
drop' n (h:t) = drop' (n - 1) t

-- 8)

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x , y) : zip' xs ys

-- 9)
elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = if x == y then True
                 else elem' x ys

-- 10)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x

-- 11) 
intersperse' :: a -> [a] -> [a]
intersperse' _ []     = []
intersperse' x (y:ys) = y : x : intersperse' x ys

-- 12)
group' :: Eq a => [a] -> [[a]]
group' [] = []
group' (h:t) = aux [h] t
              where
                aux n [] = [n]
                aux n (x:xs) | elem' x n = aux (x:n) xs
                             | otherwise  = n : aux [x] xs 

-- 13)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = (x ++ concat' xs)

-- 14)
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

-- 15)
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' l = [l] ++ tails' (tail l)

-- 16)
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] _ = True
isPrefixOf' (x:xs) (y:ys) = if x == y 
                            then isPrefixOf' xs ys
                            else False

-- 17)
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' [] _ = True
isSuffixOf' l1 l2 = if last l1 == last l2
                            then isSuffixOf' (init l1) (init l2)
                            else False 

-- 18)
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] _ = True
isSubsequenceOf' _ [] = False
isSubsequenceOf' (x:xs) (y:ys) = if x == y && isSubsequenceOf' xs ys || isSubsequenceOf' (x:xs) ys
                         then True
                         else False

-- 19)
elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' _ [] = []
elemIndices' x (h:t)  = aux 0 x (h:t)
                  where
                    aux _ _ [] = []
                    aux i x (h:t) = if x==h
                                    then i:aux (i+1) x t
                                    else aux (i+1) x t

-- 20)
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (h:t) = if h `elem` t then nub' t else h : nub' t

-- 21)
delete' :: Eq a => a -> [a] -> [a]
delete' _ [] = []
delete' n (x:xs)
                | n == x = xs
                | otherwise = x:delete' n xs

-- ou 

delete'' :: Eq a => a -> [a] -> [a]
delete'' n [] = []
delete'' n (x:xs) = aux 0 n (x:xs)
        where 
             aux i n [] = []
             aux i n (x:xs) = if n == x && i == 0 
                                 then xs
                              else x: aux i n xs
-- 22)
remove' :: Eq a => [a] -> [a] -> [a] 
remove' [] _     = []
remove' l []     = l 
remove' l (x:xs) = remove' (delete' x l) xs

-- 23) 
union' :: Eq a => [a] -> [a] -> [a]
union' l []  = l 
union' [] l  = l
union' l (y:ys) 
               | y `elem` l = union' l ys
               | otherwise = union' (l ++ [y]) ys

-- 24)
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _   = []
intersect' (h:t) l = if h `elem` l then h : intersect' t l
                     else intersect' t l

-- 25) 
insert' :: Ord a => a -> [a] -> [a]
insert' _ [] = []
insert' n (h:t) = if n > h then h:insert' n t
                  else n:(h:t)

-- 26)
unwords' :: [String] -> String
unwords' [] = ""
unwords' (h:t) = h ++ (if null t then "" else " ") ++ unwords' t

-- 27)
unlines' :: [String] -> String
unlines' [] = ""
unlines' (h:t) = h ++ "\n" ++ unlines' t

-- 28)
pMaior' :: Ord a => [a] -> Int
pMaior' [] = error "Lista Vazia"
pMaior' [x] = 0
pMaior' (h:t) 
             | h > (t !! pMaior' t) = 0
             | otherwise = 1 + pMaior' t

-- 29)
temRepetidos' :: Eq a => [a] -> Bool
temRepetidos' [] = False
temRepetidos' (h:t) = h `elem` t || temRepetidos' t

-- 30)
algarismos' :: [Char] -> [Char]
algarismos' [] = []
algarismos' (h:t)
                 | h `elem` ['0'..'9'] = h:algarismos' t
                 | otherwise = algarismos' t

-- 31)
posImpares' :: [a] -> [a]
posImpares' [] = []
posImpares' [x] = []
posImpares' (h:s:t) = s:posImpares' t

-- 32)
posPares' :: [a] -> [a]
posPares' [] = []
posPares' [x] = [x]
posPares' (h:s:t) = h:posPares' t

-- 33)
isSorted' :: Ord a => [a] -> Bool
isSorted' [] = False
isSorted' [x] = True
isSorted' (h:t) = if h <= head t then isSorted' t
                  else False

-- 34)
iSort' :: Ord a => [a] -> [a]
iSort' [] =  []
iSort' [x] = [x]
iSort' (h:t) = insert h (iSort' t)

-- 35)
menor' :: String -> String -> Bool
menor' "" _ = True
menor' _ "" = False
menor' (x:xs) (y:ys) = if x < y 
                           then True
                       else 
                           if x == y 
                               then menor' xs ys 
                           else False

-- 36)
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' _ [] = False
elemMSet' a ((x,y):t) = if a == x then True
                        else elemMSet' a t 
-- 37)
lengthMSet' :: [(a,Int)] -> Int
lengthMSet' [] = 0
lengthMSet' ((x,y):t) = y + lengthMSet' t

-- 38)
converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((x,y):t) = if y == 1 then x : converteMSet' t
                          else x : converteMSet' ((x,y-1):t)

-- 39)
insereMSet' :: Eq a => a ->  [(a,Int)] -> [(a,Int)]
insereMSet' n [] = []
insereMSet' n ((x,y):t) = if n == x 
                              then ((x,y+1):t)
                          else ((x,y):insereMSet' n t)

-- 40)
removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' n [] = []
removeMSet' n ((x,y):t) = if n == x 
                               then t 
                          else (x,y):removeMSet' n t

-- 41)
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' l = aux 1 l
      where 
           aux i [x] = [(x,i)]
           aux i (h:s:t) = if h == s 
                              then aux (i+1) (s:t)
                           else (h,i):aux 1 (s:t)

-- 42)
-- partitionEithers' :: [Either a b] -> ([a],[b])
-- partitionEithers' 
-- foda-se isto é uma merda
partitionEithers ::  [Either a b] -> ([a],[b])
partitionEithers l = (partitionLefts l, partitionRights l)
    where partitionLefts [] = []
          partitionLefts ((Left x):ls) = x:partitionLefts ls
          partitionLefts ((Right _):ls) = partitionLefts ls
          partitionRights [] = []
          partitionRights ((Left _):ls) = partitionRights ls
          partitionRights ((Right x):ls) = x:partitionRights ls

partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' [] = ([],[])
partitionEithers' ((Left a):t) = (a:as,bs)
    where (as,bs) = partitionEithers' t
partitionEithers' ((Right b):t) = (as,b:bs)
    where (as,bs) = partitionEithers' t  

-- 43)
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (m:ms) = case m of Nothing -> catMaybes ms
                             Just x -> x:catMaybes ms

-- 44)
data Movimento = Norte | Sul | Este | Oeste
                 deriving Show
posicao' :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao' (x,y) [] = (x,y)
posicao' (x,y) (h:t) = posicao' (case h of Norte -> (x,y+1)
                                           Sul -> (x,y-1)
                                           Este -> (x+1,y)
                                           Oeste -> (x-1,y)) t

-- 45)
caminho' :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho' (x1,y1) (x2,y2) | x2 > x1 = Este : caminho' (x1+1,y1) (x2,y2)
                         | x2 < x1 = Oeste : caminho' (x1-1,y1) (x2,y2)
                         | y2 > y1 = Norte : caminho' (x1,y1+1) (x2,y2)
                         | y2 < y1 = Sul : caminho' (x1,y1-1) (x2,y2)
                         | otherwise = []

-- 46)
vertical' :: [Movimento] -> Bool
vertical' [] = True
vertical' (h:t) = case h of Este -> False
                            Oeste -> False
                            _ -> vertical' t

-- 47
data Posicao = Pos Int Int
               deriving Show

maisCentral' :: [Posicao] -> Posicao
maisCentral' [] = error "Lista Vazia"
maisCentral' [Pos x y] = Pos x y
maisCentral' ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) 
                                            then maisCentral' (Pos x y : ps) 
                                        else maisCentral' (Pos a b : ps)

-- 48)
vizinhos' :: Posicao -> [Posicao] -> [Posicao]
vizinhos' _ [] = []
vizinhos' (Pos x y) ((Pos xv yv):ps) = if abs (x - xv) == 1 && y == yv || abs (y - yv) == 1 && x == xv 
                                       then Pos xv yv : vizinhos' (Pos x y) ps 
                                       else vizinhos' (Pos x y) ps

-- 49)
mesmaOrdenada' :: [Posicao] -> Bool
mesmaOrdenada' [] = True
mesmaOrdenada' [Pos _ _] = True
mesmaOrdenada' ((Pos _ y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada' (Pos x2 y2 : ps)

-- 50)
data Semaforo = Verde | Amarelo | Vermelho
                 deriving Show

interseccaoOK' :: [Semaforo] -> Bool
interseccaoOK' l = (aux l) <= 1
          where 
               aux [Vermelho] = 0
               aux [Verde] = 1
               aux [Amarelo] = 1
               aux (Vermelho : x) = aux x
               aux (Amarelo : x) = 1 + aux x
               aux (Verde : x) = 1 + aux x 
