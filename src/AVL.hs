{-# LANGUAGE InstanceSigs #-}

module AVL where

data Tree a = None 
            | Branch (Tree a) a (Tree a)
            deriving (Eq, Show)

type TreeZ a = (a, Context a)

-- tleft :: Tree a -> Tree a
-- tleft (Branch l _ _) = l
-- tleft None = error "tleft: called on None"

-- tright :: Tree a -> Tree a
-- tright (Branch _ _ r) = r
-- tright None = error "tright: called on None"

data Context a = Context (Tree a) (Tree a) [(Dir, a, Tree a)]
               deriving (Eq, Show)

data Dir = L 
         | R 
         deriving (Eq, Show)

mktz :: Tree a -> TreeZ a
mktz (Branch l a r) = (a, Context l r [])

left :: TreeZ a -> TreeZ a
left (a, Context (Branch ll la lr) r ts) = (la, Context ll lr $ (L, a, r):ts)

right :: TreeZ a -> TreeZ a
right (a, Context l (Branch rl ra rr) ts) = (ra, Context rl rr $ (R, a, l):ts)

up :: TreeZ a -> TreeZ a
up (a, Context l r ((dir, oa, ot):ts)) =
  case dir of
    L -> (oa, Context (Branch l a r) ot ts)
    R -> (oa, Context ot (Branch l a r) ts)

untz :: TreeZ a -> Tree a
untz (a, Context l r []) = Branch l a r
untz tz                  = untz $ up tz



class BST m where
  add :: (Ord a) => m a -> a -> m a
  del :: (Ord a) => m a -> a -> m a
  bempty :: (Ord a) => m a



data Node a = Node { 
  nelem :: a, 
  nheight :: Int, 
  ncounter :: Int
}

mkTree :: a -> Tree (Node a)
mkTree a = Branch None (Node a 1 1) None

incCounter :: Node a -> Node a
incCounter node = Node (nelem node) (nheight node) (ncounter node + 1)


theight :: Tree (Node a) -> Int
theight None = 0
theight (Branch _ node _) = nheight node

zfixheight :: TreeZ (Node a) -> TreeZ (Node a)
zfixheight (node, Context l r list) = (fixed, Context l r list)
  where lh = theight l
        rh = theight r
        fixed = Node (nelem node) (1 + max lh rh) (ncounter node)

tfixheight :: Tree (Node a) -> Tree (Node a)
tfixheight t = untz $ zfixheight $ mktz t



newtype AVL a = AVL (Tree (Node a))

instance BST AVL where
  add :: (Ord a) => AVL a -> a -> AVL a
  add (AVL tree) el = AVL $ balance $ insert el $ mktz tree
  del :: (Ord a) => AVL a -> a -> AVL a
  del = undefined
  bempty :: (Ord a) => AVL a
  bempty = AVL None

data Balance = BRL | RL | Ok | RR | BRR

isbalanced :: (Node a, Context (Node a)) -> Balance
isbalanced (_, Context l r _) 
  | abs (theight l - theight r) <= 1 = Ok
  | theight l > theight r = undefined
  | otherwise = undefined

balance :: TreeZ (Node a) -> Tree (Node a)
balance tz = untz $ bups tz
  where bups = undefined -- balanced ups...

-- https://upload.wikimedia.org/wikipedia/commons/3/31/Tree_rotation_animation_250x250.gif
rotateRight :: TreeZ (Node a) -> TreeZ (Node a)
rotateRight (node, Context l r list) = zfixheight (a, Context alpha newB list)
  where b = node
        Branch alpha a beta = l
        gamma = r
        newB = tfixheight $ Branch beta b gamma

rotateLeft :: TreeZ (Node a) -> TreeZ (Node a)
rotateLeft (node, Context l r list) = zfixheight (b, Context alpha gamma list)
  where a = node
        alpha = l
        Branch beta b gamma = r
        newA = tfixheight $ Branch alpha a beta


insert :: Ord a => a -> TreeZ (Node a) -> TreeZ (Node a)
insert elem (node, cntx) 
  | elem == nelem node = (incCounter node, cntx)
  | elem < nelem node = 
    case cntx of
      Context None r list -> left (node, Context l r list)
        where l = mkTree elem
      _ -> insert elem $ left (node, cntx)
  | otherwise = 
    case cntx of
      Context l None list -> right (node, Context l r list)
        where r = mkTree elem
      _ -> insert elem $ right (node, cntx)