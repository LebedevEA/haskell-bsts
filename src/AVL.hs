{-# LANGUAGE InstanceSigs #-}

module AVL (
  BST,
  AVL
) where

import BST

newtype AVL a = AVL (Tree (Node a))
              deriving (Eq,Show)

instance BST AVL where
  add :: (Ord a) => AVL a -> a -> AVL a
  add (AVL None) el = AVL $ mkTree el
  add (AVL tree) el = AVL $ balance $ insert el $ mktz tree
  
  size :: (Ord a) => AVL a -> Int
  size (AVL None) = 0
  size (AVL (Branch _ node _)) = nsize node

  bstempty :: (Ord a) => AVL a
  bstempty = AVL None
  
  kthelem :: (Ord a) => Int -> AVL a -> a
  kthelem k (AVL tree)
    | (tsize l < (k + 1)) &&
      (tsize l + ncounter (root tree) >= (k + 1)) 
        = nelem $ root tree 
    | tsize l >= (k + 1) 
        = kthelem k (AVL $ tleft tree)
    | otherwise 
        = kthelem (k - tsize l - ncounter (root tree)) (AVL $ tright tree)
        where l = tleft tree

data Balance = BRL | RL | Ok | RR | BRR
             deriving (Eq, Show)

howbalanced :: (Node a, Context (Node a)) -> Balance
howbalanced (_, Context l r _) 
  | abs (theight l - theight r) <= 1 
    = Ok
  | theight l > theight r
    = if theight (tleft l) > theight (tright l)
      then RR
      else BRR
  | otherwise
    = if theight (tright r) > theight (tleft r)
      then RL
      else BRL

fixbalance :: TreeZ (Node a) -> TreeZ (Node a)
fixbalance tz@(node, Context l r list) =
  case howbalanced tz of
    BRL -> zrotateLeft (node, Context l (trotateRight r) list)
    RL -> zrotateLeft tz
    Ok -> tz
    RR -> zrotateRight tz
    BRR -> zrotateRight (node, Context (trotateLeft l) r list)

balance :: TreeZ (Node a) -> Tree (Node a)
balance tz = untz $ bups tz
  where bups z@(_, Context _ _ []) = zfixheight $ fixbalance z
        bups z = bups $ up $ zfixheight $ fixbalance z

-- https://upload.wikimedia.org/wikipedia/commons/3/31/Tree_rotation_animation_250x250.gif
zrotateRight :: TreeZ (Node a) -> TreeZ (Node a)
zrotateRight (node, Context l r list) = zfixheight (a, Context alpha newB list)
  where b = node
        Branch alpha a beta = l
        gamma = r
        newB = tfixheight $ Branch beta b gamma

trotateRight :: Tree (Node a) -> Tree (Node a)
trotateRight t = untz $ zrotateRight $ mktz t

zrotateLeft :: TreeZ (Node a) -> TreeZ (Node a)
zrotateLeft (node, Context l r list) = zfixheight (b, Context newA gamma list)
  where a = node
        alpha = l
        Branch beta b gamma = r
        newA = tfixheight $ Branch alpha a beta

trotateLeft :: Tree (Node a) -> Tree (Node a)
trotateLeft t = untz $ zrotateLeft $ mktz t
