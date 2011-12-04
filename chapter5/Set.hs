module Set(
Set,
emptySet,
setEmpty,
inSet,
addSet,
delSet)
where

import Data.List

-- show function for all implementations expect bit vector
instance (Show a) => Show (Set a) where
	showsPrec _ (St s) str = showSet s str

showSet []	str = showString "{}" str
showSet (x:xs)  str = showChar '{' (shows x (showl xs str))
	where showl [] 	str = showChar '}' str
	      showl (x:xs) str = showChar ',' (shows x (showl xs str))

-- list implementations --
--- Unordered lists with duplicates ---

--newtype Set a =St [a]
--
--emptySet :: Set a
--emptySet = St []
--
--setEmpty :: Set a -> Bool
--setEmpty (St []) = True
--setEmpty (St _) = False
--
--inSet :: (Eq a) => a -> Set a -> Bool
--inSet x (St xs) = elem x xs
--
--addSet :: (Eq a) => a -> Set a -> Set a
--addSet x (St xs) = St (x:xs)
--
--delSet :: (Eq a) => a -> Set a -> Set a 
--delSet x (St xs) = St (filter (/=x) xs)


--- Unordered lists without duplicates ---

newtype Set a = St [a]

emptySet :: Set a 
emptySet = St []

setEmpty :: Set a -> Bool
setEmpty (St []) = True
setEmpty (St _) = False

inSet :: (Eq a) => a -> Set a -> Bool
inSet x (St xs) = elem x xs

addSet :: (Eq a) => a -> Set a -> Set a
addSet x (s@(St xs)) | inSet x s = s
		     | otherwise = St (x:xs)

delSet :: (Eq a) => a -> Set a -> Set a
delSet x (St xs) = St (delete x xs)

--- Ordered lists without duplicates ---

--newtype Set a = St [a]

--emptySet :: Set a
--emptySet = St []
--
--setEmpty :: Set a -> Bool
--setEmpty (St []) = False
--setEmpty (St _) = True
--
--inSet :: (Ord a) => a -> Set a -> Bool
--inSet x (St xs) = elem x (takeWhile (<=x) xs)
--
--addSet :: (Ord a) => a -> Set a -> Set a
--addSet x (St xs) = St (add x xs)
--	where add x []		= [x]
-- 	      add x (s@(y:ys)) | x < y	   = x:s
--	      		       | x > y	   = y:(add x ys)
--			       | otherwise = s
--
--delSet :: (Ord a) => a -> Set a -> Set a
--delSet x (St xs) = St (del x xs)
--	where del x []	= []
--	      del x (s@(y:ys)) | x < y     = s
--			       | x > y 	   = y:(del x ys)
--			       | otherwise = ys




-- Bit vector representation --
-- The efficiency of this representation depends largely on the compiler and The
-- architecture used. This representation rests upon the assumption that division
-- and module operation for powers of 2 and parity tests are implemented
-- efficiently using shifts and binary masks on the binary representation of the 
-- integer. Only a small range of Ints can be used and this implementation check
-- that elements are within this range.


--newtype Set  = St Int
--
--maxSet = truncate (logBase 2 (fromIntegral (maxBound::Int))) -1
--
--emptySet :: Set 
--emptySet = St 0
--
--setEmpty :: Set -> Bool
--setEmpty (St n) = n==0
--
--inSet :: Int -> Set -> Bool
--inSet i (St s)
--	| (i>=0) && (i<=maxSet) = odd (s `div` (2^i))
--  	| otherwise		= error ("inSet: illegal element =" ++ show i)
--
--addSet :: Int -> Set -> Set
--addSet i (St s)
--	| (i>=0) && (i<=maxSet) = St (d'*e+m)
--	| otherwise		= error ("inSet: illegal element =" ++ show i)				
--	where (d,m) = divMod s e
--	      e     = 2^i
--	      d'    = if odd d then d else d+1
--
--delSet :: Int -> Set -> Set
--delSet i (St s)
--	| (i>=0) && (i<=maxSet) = St (d'*e+m)	
--	| otherwise		= error ("inSet: illegal element =" ++ show i)
--	where (d,m) = divMod s e
--	      e     = 2^i
--	      d'    = if odd d then d-1 else d
--
--instance  Show Set where
--	showsPrec _ s str = showSet (set2List s) str
--
--set2List (St s) = s21 s 0
--	where s21 0 _ 			= []
--	      s21 n i | odd n 		= i : s21 (n `div` 2) (i+1)
--	      s21 n i | otherwise	= s21 (n `div` 2) (i+1)
--
--showSet []	str = showString "{}" str
--showSet (x:xs)  str = showChar '{' (shows x (showl xs str))
--	where showl [] 	str = showChar '}' str
--	      showl (x:xs) str = showChar ',' (shows x (showl xs str))
