{-# LANGUAGE InstanceSigs #-}
module Treap (
  BST,
  Treap
) where

import BST hiding (Node, nelem, nsize, ncounter, tsize, mkTree)
import System.Random hiding ( split )

data Node a = Node {
  nelem :: a,
  nprior :: Int,
  nsize :: Int,
  ncounter :: Int
} deriving (Show,Eq)

mkTree :: StdGen -> a -> (Tree (Node a),StdGen)
mkTree gen a = (Branch None (Node a rand 1 1) None,gen')
  where (rand,gen') = random gen

tinccnt :: Tree (Node a) -> Tree (Node a)
tinccnt (Branch None (Node e p _ c) None) 
  = Branch None (Node e p (c + 1) (c + 1)) None

tsize :: Tree (Node a) -> Int
tsize None = 0
tsize (Branch _ node _) = nsize node

newtype Treap a = Treap (Tree (Node a),StdGen)
                deriving (Show,Eq)

instance BST Treap where
  add :: (Ord a) => Treap a -> a -> Treap a
  add (Treap (None,gen)) a = Treap $ mkTree gen a
  add (Treap (tree,gen)) a =
      case mid of
        None -> Treap (merge (merge l nt) r, gen')
        _ -> Treap (merge (merge l (tinccnt mid)) r, gen)
    where (l,r') = split (<) a tree
          (mid,r) = split (<=) a r'
          (nt,gen') = mkTree gen a
          
  size :: (Ord a) => Treap a -> Int
  size (Treap (None,_)) = 0
  size (Treap (Branch _ node _,_)) = nsize node

  bstempty :: (Ord a) => Treap a
  bstempty = Treap (None,mkStdGen 5338)
  
  kthelem :: (Ord a) => Int -> Treap a -> a
  kthelem k (Treap (tree,gen))
      | (tsize l < (k + 1)) &&
      (tsize l + ncounter (root tree) >= (k + 1)) 
        = nelem $ root tree 
    | tsize l >= (k + 1) 
        = kthelem k (Treap (tleft tree,gen))
    | otherwise 
        = kthelem (k - tsize l - ncounter (root tree)) (Treap (tright tree,gen))
        where l = tleft tree
  
 
fixroot :: Tree (Node a) -> Tree (Node a)
fixroot (Branch l root r) = 
  Branch l nr r
    where nr = Node (nelem root) 
                    (nprior root) 
                    (ncounter root + tsize l + tsize r)
                    (ncounter root)

split :: Ord a => (a -> a -> Bool ) -> a -> Tree (Node a) 
                  -> (Tree (Node a), Tree (Node a))
split _ _ None = (None, None)
split cmp x (Branch l a r) =
  if nelem a `cmp` x
  then (fixroot (Branch l a rl), rr)
  else (ll, fixroot (Branch lr a r))
    where (rl, rr) = split cmp x r
          (ll, lr) = split cmp x l

merge :: Tree (Node a) -> Tree (Node a) -> Tree (Node a)
merge None t = t
merge t None = t
merge l@(Branch ll la lr) r@(Branch rl ra rr) =
  if nprior la < nprior ra
  then fixroot (Branch ll la (merge lr r)) 
  else fixroot (Branch (merge l rl) ra rr)
