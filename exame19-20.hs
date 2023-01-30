-- 1)

-- a)
inits' :: [a] -> [[a]]
inits' [] = []
inits' [x] = [[],[x]]
inits' l = inits' (init l) ++ [l]

-- b)
isPrefixOf' :: Eq a => [a] -> [a] -> Bool
isPrefixOf' [] [] = True
isPrefixOf' [] l2 = True
isPrefixOf' l1 [] = False
isPrefixOf' (x:xs) (y:ys) = if x == y then isPrefixOf' xs ys
								else False


-- 2)
data BTree a = Empty | Node a (BTree a) (BTree a)
			deriving Show


-- tree :: BTree Int tree = Node 5 (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty)) (Node 8 (Node 7 (Node 6 Empty Empty) Empty) (Node 9 Empty Empty))

{- 
		  5
	   /    \ 
      2      8
     / \    / \
    1   3  7   9
          /
         6 
-}

-- a)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1 
folhas (Node r e d) = (folhas e) + (folhas d)

-- b)
path :: [Bool] -> BTree a -> [a]
path [] (Node r e d) = [r]
path _ Empty = []
path (h:t) (Node r e d) = r : if h == False then path t e
								else path t d


-- 3)
type Polinomio = [Coeficiente]
type Coeficiente = Float


-- a)



-- 4)
type Mat a = [[a]]


-- a)
quebraLinha :: [Int] -> [a] -> [[a]]
quebraLinha [] l2 = []
quebraLinha (h:t) l2 = take h l2 : quebraLinha t (drop h l2)

-- b)
fragmenta :: [Int] -> [Int] -> Mat a -> [Mat a]


