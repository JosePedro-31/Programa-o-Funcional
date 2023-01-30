-- 1)

data BTree a = Empty
		| Node a (BTree a) (BTree a)
	deriving Show


-- a)
altura :: BTree a -> Int
altura Empty = 0
altura (Node r esq dir) = 1 + max (altura esq) (altura dir)


-- b)
contaNodos :: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node r e d) = 1 + contaNodos (e) + contaNodos (d)


-- c)
folhas :: BTree a -> Int
folhas Empty = 0
folhas (Node r Empty Empty) = 1
folhas (Node r e d) = folhas (e) + folhas (d)


-- d)
prune :: Int -> BTree a -> BTree a
prune x Empty = Empty
prune x (Node r e d) | x == 0 = Empty
					 | otherwise = Node r (prune (x-1) e) (prune (x-1) d)


-- e)
path :: [Bool] -> BTree a -> [a]
path [] (Node r e d) = [r]
path (h:t) Empty = []
path (h:t) (Node r e d) = r : if h == False then path t (e)
								else path t (d)


-- f)
mirror :: BTree a -> BTree a
mirror Empty = Empty
mirror (Node r e d) = Node r (mirror d) (mirror e)


-- g)
zipWithBT :: (a->b->c) -> BTree a -> BTree b -> BTree c
zipWithBT f Empty _ = Empty
zipWithBT f _ Empty = Empty
zipWithBT f (Node r1 e1 d1) (Node r2 e2 d2) = Node (f r1 r2) (zipWithBT f e1 e2) (zipWithBT f d1 d2)


-- h)
unzipBT :: BTree (a,b,c) -> (BTree a, BTree b, BTree c)
unzipBT Empty = (Empty, Empty, Empty)
unzipBT (Node (ra,rb,rc) e d) = (Node ra re1 rd1, Node rb re2 rd2, Node rc re3 rd3)

	where 
		(re1,re2,re3) = unzipBT e
		(rd1,rd2,rd3) = unzipBT d


-- 2)


-- a)
minimo :: Ord a => BTree a -> a
minimo (Node r Empty _) = r
minimo (Node r Empty Empty) = r
minimo (Node r e d) = minimo e


-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node r Empty Empty) = Empty
semMinimo (Node r Empty d) = d
semMinimo (Node r e d) = Node r (semMinimo e) d


-- c)


-- d)


-- 3)
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
				   | Rep
				   | Faltou
		deriving Show

type Turma = BTree Aluno -- árvore binária de procura (ordenada por número)


-- a)




