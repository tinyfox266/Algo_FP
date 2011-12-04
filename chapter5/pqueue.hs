module PQueue (
PQueue,
emptyPQ,
pqEmpty,
enPQ,
dePQ,
frontPQ)
where

newtype PQueue a = PQ [a]
	deriving Show

emptyPQ :: PQueue a
emptyPQ = PQ []

pqEmpty :: PQueue a -> Bool
pqEmpty (PQ []) = True
pqEmpty (PQ _) = False

enPQ :: (Ord a) => a -> PQueue a -> PQueue a
enPQ x (PQ q) = PQ (insert x q)
	where insert x []			= [x]
	      insert x (r@(e:r')) | x <= e 	= x:r
	      			  | otherwise 	= e:(insert x r') 

dePQ :: (Ord a) => PQueue a -> PQueue a 
dePQ (PQ []) = error "dePQ: empty priority queue"
dePQ (PQ (x:xs)) = PQ xs

frontPQ :: (Ord a) => PQueue a -> a
frontPQ (PQ []) = error "frontPQ: empty priority queue"
frontPQ (PQ (x:xs)) = x
