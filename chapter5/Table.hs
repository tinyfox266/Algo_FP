module Table (
Table,
newTable,
findTable,
updTable,
)
where
import Array

-- Implement a table as a function
--newtype Table a b = Tbl (b->a)
--
--newTable :: (Eq b) => [(b,a)] -> Table a b
--newTable assocs	= foldr updTable 
--			(Tbl (\_ -> error "updTable: item not found"))
--			assocs
--
--findTable :: (Eq b) => Table a b -> b -> a
--findTable (Tbl f) i = f i
--
--updTable :: (Eq b) => (b,a) -> Table a b -> Table a b
--updTable (i,x) (Tbl f) = Tbl g
--	where g j | j == i    = x
--	      g j | otherwise = f j
--
--instance Show (Table a b) where
--	showsPrec _ _ str = showString "<<A Table>>" str

-- List implementation 

--newtype Table a b = Tbl [(b,a)]
--
--newTable :: (Eq b) => [(b,a)] -> Table a b
--newTable assocs	= Tbl assocs
--
--findTable :: (Eq b) => Table a b -> b -> a
--findTable (Tbl []) i = error "findTable: item not found in table"
--findTable (Tbl ((j,v):r)) i
--	| j == i 	= v
--	| otherwise 	= findTable (Tbl r) i
--
--updTable :: (Eq b) => (b,a) -> Table a b -> Table a b
--updTable e (Tbl []) = Tbl [e]
--updTable (e'@(i,_)) (Tbl (e@(j,_):r))
--	| i == j	= Tbl (e':r)
--	| otherwise	= Tbl (e:r')
--	where Tbl r' = updTable e' (Tbl r)
--
--instance (Show a,Show b) => Show (Table a b) where
--	showsPrec _ (Tbl l) str = shows l str

-- Array Implementation
-- If the type of the index is restricted to the class Ix, tables can be Implemented
-- as arrays. Another restriction is that it is not possible to insert a new 
-- value outside the boundaries of the first table.

newtype Table a b = Tbl (Array b a)

newTable :: (Ix b) => [(b,a)] -> Table a b
newTable l		= Tbl (array (lo,lh) l)
	where 	indices	= map fst l
		lo	= minimum indices
		lh	= maximum indices			

findTable :: (Ix b) => Table a b -> b -> a
findTable (Tbl a) i = a ! i

updTable :: (Ix b) => (b,a) -> Table a b -> Table a b
updTable p@(i,x) (Tbl a) = Tbl (a // [p])

instance (Ix b,Show a,Show b) => Show (Table a b) where
	showsPrec _ (Tbl a) str = shows (assocs a) str
