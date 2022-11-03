-- 1)
myenumFromTo :: Int -> Int -> [Int]
myenumFromTo x y | x == y = [x]
			   | x > y = []
			   | otherwise = x : myenumFromTo (x+1) y

-- 2)
myenumFromThenTo :: Int -> Int -> Int -> [Int]
myenumFromThenTo x y z | x == z = [x]
                       | x > z = []
                       | otherwise = x : myenumFromThenTo y (y+(y-x)) z


-- 3)
concatenar :: [a] -> [a] -> [a]
concatenar [] l2 = l2
concatenar (h:t) l2 = h : concatenar t l2  


-- 4)
calcula :: [a] -> Int -> a
calcula (h:t) i = if i == 0 then h
                else calcula t (i-1)


-- 5)
myReverse :: [a] -> [a]
myReverse [] = []
myReverse l = last l : myReverse (init l)

myReverse' :: [a] -> [a]
myReverse' [] = []
myReverse' (h:t) = myReverse' t ++ [h]


-- 6)
myTake :: Int -> [a] -> [a]
myTake n (h:t) | n == 0 = []
               | n > length(h:t) = (h:t)
               | otherwise = h : myTake (n-1) t


-- 7)
myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n (h:t) | n == 0 = (h:t)
               | otherwise = myDrop (n-1) t


-- 8)
myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip (xs) (ys)


-- 9)
myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n-1) x


-- 10)
myIntersperse :: a -> [a] -> [a]
myIntersperse _ [] = []
myIntersperse n [x] = [x]
myIntersperse n (h:t) = h : n : myIntersperse n t


-- 11)
myGroup :: Eq a => [a] -> [[a]]
myGroup [] = []
myGroup (h:t) = (h : takeWhile(==h) t) : myGroup (dropWhile(==h) t)


-- 12)
myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ myConcat xs


-- 13)
myInits :: [a] -> [[a]]
myInits [] = []
myInits [x] = [[],[x]]
myInits l = myInits (init l) ++ [l]


-- 14)
myTails :: [a] -> [[a]]
myTails [] = [[]]
myTails [x] = [[x],[]]
myTails l = [l] ++ myTails (init l) 


-- 15)
myHeads :: [[a]] -> [a]
myHeads [] = []
myHeads ([]:tl) = myHeads tl
myHeads (l1:tl) = head l1 : myHeads tl


-- 16)
myTotal :: [[a]] -> Int
myTotal [] = 0
myTotal (l1:t) = length l1 + myTotal t


-- 17)
myFun :: [(a,b,c)] -> [(a,c)]
myFun [] = []
myFun ((x,y,z):t) = (x,z) : myFun t


-- 18)
myCola :: [(String,b,c)] -> String
myCola [] = ""
myCola ((s,b,c):t) = s ++ myCola t


-- 19)
myIdade :: Int -> Int -> [(String,Int)] -> [String]
myIdade _ _ [] = []
myIdade ano idade ((s,n):t) | ano-n >= idade = s : myIdade ano idade t
                            | otherwise = myIdade ano idade t


-- 20)
myPowerEnumFrom :: Int -> Int -> [Int]
myPowerEnumFrom n m = aux 0 n m
        where
                aux _ _ 0 = []
                aux i n m = if i <= (m-1) then n^i : aux (i+1) n m
                            else []


-- 21)
{-
isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = aux n m 

    where
        aux n m = if m >= 2 && m <= (sqrt n) && (mod n m) = 0 then False
                else True
-}

-- 22)
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = if x == y then isPrefixOf xs ys
                            else False


-- 23)
isSufixOf :: Eq a => [a] -> [a] -> Bool
isSufixOf [] l2  = True
isSufixOf l1 [] = False
isSufixOf l1 l2 = if last l1 == last l2 then isSufixOf (init l1) (init l2)
                else False


-- 24)
isSubsequenceOf :: Eq a => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) = if x == y then isSubsequenceOf xs ys 
                                else isSubsequenceOf (x:xs) ys


-- 25)
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n [] = []
elemIndices n (h:t) = aux 0 n (h:t)

    where
        aux _ _ [] = []
        aux i n (h:t) = if n == h then i : aux (i+1) n t
                        else aux (i+1) n t


-- 26)
myNub :: Eq a => [a] -> [a]
myNub [] = []
myNub [x] = [x]
myNub (h:t) = if elem h t then myNub t
            else h : myNub t


-- 27)
myDelete :: Eq a => a -> [a] -> [a]
myDelete n [] = []
myDelete n (h:t) = if n == h then t
                else h : myDelete n t


-- 28)
remove :: Eq a => [a] -> [a] -> [a]
remove [] _ = []
remove l1 [] = l1
remove (x:xs) (y:ys) = remove (myDelete x (y:ys)) xs


-- 29)
myUnion :: Eq a => [a] -> [a] -> [a]
myUnion [] l2 = l2
myUnion l1 [] = l1
myUnion l1 (h:t) = if elem h l1 then myUnion l1 t
                    else h : myUnion l1 t


-- 30)
myIntersect :: Eq a => [a] -> [a] -> [a]
myIntersect l [] = [] 
myIntersect [] l = []
myIntersect (h:t) l = if elem h l then h : myIntersect t l
                    else myIntersect t l


-- 31)
myInsert :: Ord a => a -> [a] -> [a]
myInsert n [] = [n]
myInsert n (h:t) = if n <= h then n:h:t
                    else h : myInsert n t


-- 32)
myUnwords :: [String] -> String
myUnwords [] = ""
myUnwords [x] = x
myUnwords (h:t) = h ++ " " ++ myUnwords t


-- 33)
myUnlines :: [String] -> String
myUnlines [] = ""
myUnlines (h:t) = h ++ "\n" ++ myUnlines t


-- 34)
pMaior :: Ord a => [a] -> Int
pMaior [] = error "LISTA VAZIA"
pMaior (h:t) = aux 0 (h:t)
    
    where  
        aux i [x] = i
        aux i (h:s:t) = if h >= s then aux i (h:t)
                        else aux (i+1) (s:t)


-- 35)
myLookup :: Eq a => a -> [(a,b)] -> Maybe b
myLookup n [] = Nothing
myLookup n ((a,b):t) = if n == a then Just b 
                        else myLookup n t


-- 36)
preCrescente :: Ord a => [a] -> [a]
preCrescente [] = []
preCrescente (h:s:t) = if h <= s then h : preCrescente (s:t)
                        else [h]


-- 37)
iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (h:t) = myInsert h (iSort t)


-- 38)
menor :: String -> String -> Bool
menor [] [] = False
menor s1 [] = False
menor [] s2 = True
menor (x:xs) (y:ys) = if x <= y then menor xs ys
                    else False


-- 39)
elemMSet :: Eq a => a -> [(a,Int)] -> Bool
elemMSet n [] = False
elemMSet n ((a,b):t) = if n == a then True
                    else elemMSet n t


-- 40)
converteMSet :: [(a,Int)] -> [a]
converteMSet [] = []
converteMSet ((a,b):t) = if b > 0 then a : converteMSet ((a,(b-1)):t)
                        else converteMSet t


-- 41)
insereMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet n [] = []
insereMSet n ((a,b):t) = if n == a then ((a,(b+1)):t)
                        else (a,b) : insereMSet n t


-- 42)
removeMSet :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet n [] = []
removeMSet n ((a,b):t) = if n == a then 
                                if (b-1)>0 then ((a,(b-1)):t)
                                else t
                    else (a,b) : removeMSet n t


-- 43)
constroiMSet :: Ord a => [a] -> [(a,Int)]
constroiMSet [] = []
constroiMSet (h:s:t) = aux 1 (h:s:t)
        
        where
            aux i [] = []
            aux i [x] = [(x,i)]
            aux i (h:s:t) = if h == s then aux (i+1) (s:t)
                            else (h,i) : aux 1 (s:t)

-- 44)
partitionEithers :: [Either a b] -> ([a],[b])
partitionEithers [] = ([],[])
partitionEithers (h:t) = case h of
        Left a -> (a:x , y)
        Right b -> (x , b:y )
    where (x,y) = partitionEithers t


-- 45)
catMaybes :: [Maybe a] -> [a]
catMaybes (h:t) = case h of Nothing -> catMaybes t
                            Just a -> a : catMaybes t


-- 46)
data Movimento = Norte | Sul | Este | Oeste
            deriving Show

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (x,y) (xs,ys) | x > xs = Este : caminho (x-1,y) (xs,ys)
                      | x < xs = Oeste : caminho (x+1,y) (xs,ys)
                      | y > ys = Norte : caminho (x,y-1) (xs,ys)
                      | y < ys = Sul : caminho (x,y+1) (xs,ys)
                      | otherwise = []


-- 47)


-- 48)
type Ponto = (Float,Float)
data Retangulo = Rect Ponto Ponto

contaQuadrados :: [Retangulo] -> Int 
contaQuadrados [] = 0
contaQuadrados ((Rect (x,y) (xs,ys)):t) = if abs(x-xs) == abs(y-ys) then 1 + contaQuadrados t
                                            else contaQuadrados t


-- 49)
areaTotal :: [Retangulo] -> Float
areaTotal [] = 0
areaTotal ((Rect (x,y) (xs,ys)):t) = abs(x-xs) * abs(y-ys) + areaTotal t


-- 50)
data Equipamento = Bom | Razoavel | Avariado 
                deriving Show


naoReparar :: [Equipamento] -> Int
naoReparar [] = 0
naoReparar (h:t) = case h of
                Avariado -> naoReparar t
                Bom -> 1 + naoReparar t
                Razoavel -> 1 + naoReparar t