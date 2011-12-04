module AVLTree(
AVLTree,
emptyAVL,
addAVL,
)
where

data (Ord a,Show a) => AVLTree a = EmptyAVL
                                 | NodeAVL a (AVLTree a) (AVLTree a)
    deriving Show


