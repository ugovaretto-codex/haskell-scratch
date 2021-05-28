{-# OPTIONS_GHC -Wno-deferred-type-errors #-}
module SortedTree where
-- Binary Search STree Datatype (Belongs to BTsort along with add, addList and inorder)
data STree a = Node a (STree a) (STree a) | Empty

-- add an Element to the STree
add :: (a->a->Bool) -> a -> STree a -> STree a
add _ n Empty        = Node n Empty Empty
add lt n (Node v l r) = if n `lt` v then Node v (add lt n l) r else Node v l (add lt n r)

-- add a List of Elements to a STree
addList :: Ord a => (a->a->Bool) ->  STree a -> [a] -> STree a
addList lt = foldr (add lt)

-- make inorder list of a sTree
inorder :: STree a -> [a]
inorder Empty        = []
inorder (Node v l r) = inorder l ++ (v : inorder r)

-- Sort a list by adding all it's elements to a binary search STree and return it's inorder list
treeSort :: Ord a => (a->a->Bool) -> [a] -> [a]
treeSort lt = inorder . addList lt Empty 

smallest :: STree a -> a
smallest (Node v Empty _) = v
smallest (Node v l r) = smallest l

largest :: STree a -> a
largest (Node v _ Empty) = v
largest (Node v l r) = largest r 