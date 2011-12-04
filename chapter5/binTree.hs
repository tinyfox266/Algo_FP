module BinTree (
BinTree,
emptyTree,
inTree,
addTree,
delTree,
buildTree,
inorder)
where

data (Ord a) => BinTree a = EmptyBT
			  | NodeBT a (BinTree a) (BinTree a) 	
    deriving Show

emptyTree :: (Ord a) => BinTree a
emptyTree = EmptyBT

inTree :: (Ord a) => a -> BinTree a -> Bool
inTree v' EmptyBT 		           = False
inTree v' (NodeBT v lf rt) | v'==v = True
                           | v'<v  = inTree v' lf
                           | v'>v  = inTree v' rt 

addTree :: (Ord a) => a -> BinTree a -> BinTree a
addTree v' EmptyBT 			            = NodeBT v' EmptyBT EmptyBT
addTree v' (NodeBT v lf rt) | v'==v 	= NodeBT v lf rt
                            | v'<v	    = NodeBT v (addTree v' lf) rt
                            | otherwise = NodeBT v lf (addTree v' rt)

buildTree :: (Ord a) => [a] -> BinTree a
buildTree lf = foldr addTree EmptyBT lf

buildTree' :: (Ord a) => [a] -> BinTree a --imporved version
buildTree' []	= EmptyBT
buildTree' lf 	= NodeBT x (buildTree' l1) (buildTree l2)
        where l1	= take n lf
              (x:l2)= drop n lf
              n		= (length lf) `div` 2

inorder :: (Ord a) => BinTree a -> [a]
inorder EmptyBT       		= []
inorder (NodeBT v lf rt)    = (inorder lf) ++ [v] ++ (inorder rt)

inorder' :: (Ord a) => BinTree a -> [a] -- imporved version without append
inorder' t 		= inorder'' t []
	where inorder'' EmptyBT z 	       = z
	      inorder'' (NodeBT v lf rt) z = inorder'' lf (v:(inorder'' rt z))

delTree :: (Ord a) => a -> BinTree a -> BinTree a
delTree v' EmptyBT      = EmptyBT
delTree v' (nd@(NodeBT v lf EmptyBT)) 
            | v'==v     = lf

delTree v' (nd@(NodeBT v EmptyBT rt)) 
            | v'==v     = rt

delTree v' (NodeBT v lf rt)
            | v'<v      = NodeBT v (delTree v' lf) rt
            | v'>v      = NodeBT v lf (delTree v' rt)
            | otherwise = let k = minTree rt 
                          in NodeBT k lf (delTree k rt)

minTree :: (Ord a) => BinTree a -> a
minTree (NodeBT v EmptyBT _) = v
minTree (NodeBT _ lf _)      = minTree lf


