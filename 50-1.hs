--1)

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' x y | x == y = [x]
			   | x > y = []
			   | otherwise = x : enumFromTo' (x+1) y


-- 2)
enumFromThenTo' :: Int -> Int -> Int -> [Int]
enumFromThenTo' x y z | x == z = [x]
					  | x > z = []
					  | otherwise = x : enumFromThenTo' y (y+(y-x)) z


-- 3)
concatena :: [a] -> [a] -> [a]
concatena [] [] = []
concatena [] l2 = l2
concatena l1 [] = l1
concatena (h:t) l2 = h: concatena t l2
 

-- 4)
posicao :: [a] -> Int -> a 
posicao (h:t) 0 = h
posicao (h:t) n = posicao t (n-1)


-- 5)
reverse' :: [a] -> [a]
reverse' [] = []
reverse' l = last l : reverse' (init l)


-- 6)
take' :: Int -> [a] -> [a]
take' 0 l = []
take' n (h:t) = h : take' (n-1) t


-- 7)
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' n (h:t) = drop' (n-1) t


-- 8)
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = ((x,y):zip' xs ys)

--9)
replicate' :: Int -> a -> [a]
replicate' 0 x = []
replicate' n x = x : replicate (n-1) x


--10)
intersperse' :: a -> [a] -> [a]
intersperse' n [] = []
intersperse' n [x] = [x]
intersperse' n (h:t) = h : n : intersperse' n t


-- 11)


-- 12)
concat' :: [[a]] -> [a]
concat' [] = []
concat'[[x]] = [x]
concat'(x:t) = x ++ concat' t


-- 13)
inits' :: [a] -> [[a]]
inits' [] = [[]]
inits' [x] = [[],[x]]
inits' l = inits' (init l) ++ [l]


-- 14)
tails' :: [a] -> [[a]]
tails' [] = [[]]
tails' [x] = [[x],[]]
tails' l = [l] ++ tails' (tail l)


-- 15)
heads' :: [[a]] -> [a]
heads' [] = []
heads' ([]:t) = heads' t
heads' ((x:xs):t) = x : heads' t


--16)
total' :: [[a]] -> Int
total' [] = 0
total' ([]:lt) = total' lt
total' ((x:xs):lt) = 1 + total' ((xs):lt)


-- 17)
fun' :: [(a,b,c)] -> [(a,c)]
fun' [] = []
fun' ((a,b,c):t) = (a,c) : fun' t


-- 18)
cola' :: [(String,b,c)] -> String
cola' [] = ""
cola' ((s,b,c):t) = s ++ cola' t


--19)
idade' :: Int -> Int -> [(String,Int)] -> [String]
idade' ano idade [] = []
idade' ano idade ((nome,nascimento):t) = if (ano - nascimento) >= idade then nome : idade' ano idade t
										else idade' ano idade t


-- 20)
powerEnumFrom' :: Int -> Int -> [Int]
powerEnumFrom' n m = aux 0 n m 

	where
		aux i n m | m <= 0 = []
				  | i == m = []
				  | otherwise = (n^i) : aux (i+1) n m


-- 21)
-- SUPOSTAMENTE FUNCIONA
{-
isPrime' :: Int -> Bool
isPrime' n = aux 2 n
	
	where 
		aux m n | m > sqrt(n) = True
				| otherwise = if mod n m == 0 then False
							else aux (m+1) n
-}

-- 22)
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' _ [] = False
isPrefixOf' [] _ = True
isPrefixOf' (h:t) (hs:ts) = if h == hs then isPrefixOf' t ts
							else False 


-- 23)
isSuffixOf' :: Eq a => [a] -> [a] -> Bool
isSuffixOf' _ [] = False
isSuffixOf' [] _ = True
isSuffixOf' l1 l2 = if last l1 == last l2 then isSuffixOf' (init l1) (init l2)
						else False 


-- 24)
isSubsequenceOf' :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf' [] l2 = True
isSubsequenceOf' l1 [] = False
isSubsequenceOf' (x:xs) (y:ys) = if x == y then isSubsequenceOf' xs ys
									else isSubsequenceOf' (x:xs) ys 


-- 25)
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices n l = aux 0 n l

	where
		aux i n [] = []
		aux i n (h:t) = if n == h then i : aux (i+1) n t
							else aux (i+1) n t


-- 26)
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' [x] = [x]
nub' (h:t) = if elem h t then nub' t
				else h : nub' t


-- 27)
delete' :: Eq a => a -> [a] -> [a]
delete' n [] = []
delete' n (h:t) = if n == h then t
					else h : delete' n t


-- 28)
ocorrencias :: Eq a => [a] -> [a] -> [a]
ocorrencias [] _ = []
ocorrencias l [] = l
ocorrencias l1 (y:ys) = ocorrencias (deleteaux y l1) ys


	where 
		deleteaux n [] = []
		deleteaux n (h:t) | n == h = t
						  | otherwise = h : deleteaux n t 


-- 29)
union' :: Eq a => [a] -> [a] -> [a]
union' [] l2 = l2
union' l1 [] = l1
union' l1 (h:t) | elem h l1 = union' l1 t
				| otherwise = union' (l1 ++ [h]) t


-- 30)
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] _ = []
intersect' _ [] = []
intersect' (h:t) l2 = if elem h l2 then h : intersect' t l2
						else intersect' t l2


-- 31)
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (h:t) | n < h = n:h:t
				| otherwise = h : insert' n  t   


-- 32)
unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (h:t) = h ++ " " ++ unwords' t 


--33)
unlines' :: [String] -> String
unlines' [] = ""
unlines' [x] = x ++ "\n"
unlines' (h:t) = h ++ "\n" ++ unlines' t


-- 34)
pMaior :: Ord a => [a] -> Int
pMaior [] = error "LISTA VAZIA"
pMaior (h:t) = aux 0 (h:t)

	where
		aux i [x] = i
		aux i (h:s:t) | h > s = aux i (h:t)
					  | otherwise = aux (i+1) (s:t)


-- 35)
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' n ((a,b):t) = if n == a then Just b
						else lookup' n t


--36)
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente [x] = [x]
preCrescente (h:s:t) = if h <= s then h : preCrescente (s:t)
					else [h]


-- 37)
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:s:t) = if h > s then insert' h (s:t)
				else h : iSort' (s:t)


-- 38)
menor :: String -> String -> Bool
menor s1 [] = False
menor [] s2 = True
menor (x:xs) (y:ys) = if x <= y then menor xs ys
					else False


-- 39)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False
elemMSet n ((x,num):t) = if n == x then True 
						else elemMSet n t


--40)
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,num):t) = if num > 0 then a : converteMSet ((a,(num-1)):t)
							else converteMSet t


-- 41)
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = [(n,1)]
insereMSet n ((a,num):t) = if n == a then ((a,num+1):t)
						else (a,num) : insereMSet n t


-- 42)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet n [] = []
removeMSet n ((a,num):t) = if n == a then 
										if (num-1)>0 then ((a,(num-1)):t)
										else t
							else (a,num) : removeMSet n t


-- 43)
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet l = aux 1 l 

	where 
		aux i [] = []
		aux i [x] = [(x,i)]
		aux i (h:s:t) | h == s = aux (i+1) (s:t)
					  | otherwise = (h,i) : aux 1 (s:t)


-- 44)
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([],[])
partitionEithers (h:t) = case h of
	 	
	 				Left a -> (a:x , y)
	 				Right b -> (x, b:y)

	 		where
	 			(x,y) = partitionEithers' t


-- 45)
catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = [] 
catMaybes' (h:t) = case h of
				Nothing -> catMaybes' t
				Just a -> a : catMaybes' t


-- 46)
data Movimento = Norte | Sul | Este | Oeste
			deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf) | xf > xi = Este : caminho (xi+1,yi) (xf,yf)
						| xf < xi = Oeste : caminho (xi-1,yi) (xf,yf)
						| yf > yi = Norte : caminho (xi,yi+1) (xf,yf)
						| yf < yi = Sul : caminho (xi,yi-1) (xf,yf)
						| otherwise = []


-- 47)


-- 48)
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto

contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados ((Rect (x1,y1) (x2,y2)):t) = if abs (x2-x1) == abs (y2-y1) then 1 + contaQuadrados t
												else contaQuadrados t


-- 49)
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x1,y1) (x2,y2)):t) = abs(x2-x1) * abs(y2-y1) + areaTotal t


-- 50)
data Equipamento = Bom | Razoavel | Avariado
				deriving Show

naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of 
				Bom -> 1 + naoReparar t
				Razoavel -> 1 + naoReparar t
				Avariado -> naoReparar t