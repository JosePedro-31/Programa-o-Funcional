data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show)


arvore = Node 6 (Node 4 (Node 2 Empty Empty) (Node 5 Empty Empty)) (Node 8 (Node 7 Empty Empty) Empty)


-- a)
altura :: BTree a -> Int 
altura Empty = 0 
altura (Node r e d) = 1 + max(altura e) (altura d)

-- b)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = (folhas e) + (folhas d)


-- c)
prune :: Int -> BTree a -> BTree a 
prune x Empty = Empty
prune x (Node r e d) | x == 0 = Empty
					 | otherwise = Node r (prune (x-1) e) (prune (x-1) d)

-- d)
