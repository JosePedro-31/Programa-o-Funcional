-- 1)
data ExpInt = Const Int
			| Simetrico ExpInt
			| Mais ExpInt ExpInt
			| Menos ExpInt ExpInt
			| Mult ExpInt ExpInt

-- a)
calcula :: ExpInt -> Int
calcula (Const n) = n
calcula x = case x of
		Simetrico k -> (-1) * calcula k
		Mais k1 k2 -> calcula k1 + calcula k2
		Menos k1 k2 -> calcula k1 - calcula k2
		Mult k1 k2 -> calcula k1 * calcula k2


-- 2.

data RTree a = R a [RTree a]

-- a)
soma :: Num a => RTree a -> a
soma (R x []) = x
soma (R x l) = x + sum (map soma l)


-- b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum(map altura l)


-- c)
prune :: Int -> RTree a -> RTree a
prune 1 (R x l) = (R x [])
prune n (R x l) | n>1 = R x (map (prune (n-1)) l)


-- d)
mirror :: RTree a -> RTree a
mirror (R x []) = R x []
mirror (R x l) = R x (reverse (map mirror l))

-- e)
postorder :: RTree a -> [a]
postorder (R x []) = [x]
postorder (R x l) = concat (map postorder l) ++ [x]


-- 3.

data BTree a = Empty | Node a (BTree a) (BTree a)

data LTree a = Tip a | Fork (LTree a) (LTree a)

-- a)
ltSum :: Num a => LTree a -> a
ltSum (Tip x) = x
ltSum (Fork e d) = (ltSum e) + (ltSum d)


-- b)
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)


-- c)
ltHeight :: LTree a -> Int 
ltHeight (Tip x) = 1
ltHeight (Fork e d) = (max (ltHeight e) (ltHeight d)) + 1

