{-# LANGUAGE InstanceSigs #-}

module Splay (
  BST,
  Splay
) where

import BST

newtype Splay a = Splay (Tree (Node a))
                deriving (Eq,Show)

instance BST Splay where
  add :: (Ord a) => Splay a -> a -> Splay a
  add (Splay None) el = Splay $ mkTree el
  add (Splay tree) el = Splay $ splay $ insert el $ mktz tree
  size :: (Ord a) => Splay a -> Int
  size (Splay None) = 0
  size (Splay (Branch _ node _)) = nsize node
  bstempty :: (Ord a) => Splay a
  bstempty = Splay None
  kthelem :: (Ord a) => Int -> Splay a -> a
  kthelem k (Splay tree)
    | (tsize l < (k + 1)) &&
      (tsize l + ncounter (root tree) >= (k + 1)) 
        = nelem $ root tree 
    | tsize l >= (k + 1) 
        = kthelem k (Splay $ tleft tree)
    | otherwise 
        = kthelem (k - tsize l - ncounter (root tree)) (Splay $ tright tree)
        where l = tleft tree

splay :: TreeZ (Node a) -> Tree (Node a)
splay = untz . splay'
  where splay' :: TreeZ (Node a) -> TreeZ (Node a)
        splay' tz@(_, Context _ _ (u1:u2:us)) =
          splay' $ case u2 of
            (L,_,_) ->
              case u1 of
                (L,_,_) -> zigzig $ up $ up tz
                (R,_,_) -> zigzag $ up $ up tz
            (R,_,_) ->
                case u1 of
                  (L,_,_) -> zagzig $ up $ up tz
                  (R,_,_) -> zagzag $ up $ up tz
        splay' tz@(_, Context _ _ [u]) =
          splay' $ case u of
            (L,_,_) -> zig $ up tz
            (R,_,_) -> zag $ up tz
        splay' tz = tz

zag :: TreeZ (Node a) -> TreeZ (Node a) -- from right to left
zag (node, Context l r list) = zfixheight (b, Context newA gamma list)
  where a = node
        alpha = l
        Branch beta b gamma = r
        newA = tfixheight $ Branch alpha a beta

zig :: TreeZ (Node a) -> TreeZ (Node a) -- from left to right
zig (node, Context l r list) = zfixheight (a, Context alpha newB list)
  where b = node
        Branch alpha a beta = l
        gamma = r
        newB = tfixheight $ Branch beta b gamma

zigzig :: TreeZ (Node a) -> TreeZ (Node a)
zigzig = zig . up . zig . left 

zigzag :: TreeZ (Node a) -> TreeZ (Node a)
zigzag = zig . up . zag . left 

zagzag :: TreeZ (Node a) -> TreeZ (Node a)
zagzag = zag . up . zag . right 

zagzig :: TreeZ (Node a) -> TreeZ (Node a)
zagzig = zag . up . zig . right
