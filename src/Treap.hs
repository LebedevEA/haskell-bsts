module Treap (
  BST,
  Treap
) where

import BST hiding (Node, nelem, nsize, ncounter, tsize)

data Node a = Node {
  nelem :: a,
  nprior :: Int,
  nsize :: Int,
  ncounter :: Int
}

tsize :: Tree (Node a) -> Int
tsize None = error "tsize: called on None"
tsize (Branch _ node _) = nsize node

-- instance (Ord a) => Eq (Node a) where
--   u == v = nelem u == nelem v

-- instance (Ord a) => Ord (Node a) where
--   compare u v = compare (nelem u) (nelem v)

newtype Treap a = Treap (Tree (Node a))
 
fixroot :: Tree (Node a) -> Tree (Node a)
fixroot (Branch l root r) = 
  Branch l nr r
    where nr = Node (nelem root) 
                    (nprior root) 
                    (ncounter root )
                    (ncounter root)

split :: Ord a => a -> Tree (Node a) -> (Tree (Node a), Tree (Node a))
split _ None = (None, None)
split x (Branch l a r) =
  if nelem a < x
  then (fixroot (Branch l a rl), rr)
  else (ll, fixroot (Branch lr a r))
    where (rl, rr) = split x r
          (ll, lr) = split x l

merge :: Tree (Node a) -> Tree (Node a) -> Tree (Node a)
merge None t = t
merge t None = t
merge l@(Branch ll la lr) r@(Branch rl ra rr) =
  if nprior la < nprior ra
  then fixroot (Branch ll la (merge lr r)) 
  else fixroot (Branch (merge l rl) ra rr)