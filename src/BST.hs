module BST where

data Tree a = None 
            | Branch (Tree a) a (Tree a)
            deriving (Eq, Show)

type TreeZ a = (a, Context a)

tleft :: Tree a -> Tree a
tleft (Branch l _ _) = l
tleft None = error "tleft: called on None"

tright :: Tree a -> Tree a
tright (Branch _ _ r) = r
tright None = error "tright: called on None"

root :: Tree a -> a
root (Branch _ a _) = a
root None = error "root: called on None"

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
untz tz = untz $ up tz

class BST m where
  add :: (Ord a) => m a -> a -> m a
  size :: (Ord a) => m a -> Int
  -- del :: (Ord a) => m a -> a -> m a
  bstempty :: (Ord a) => m a
  kthelem :: (Ord a) => Int -> m a -> a

data Node a = Node { 
  nelem :: a, 
  nheight :: Int, 
  ncounter :: Int,
  nsize :: Int
} deriving (Eq,Show)

mkTree :: a -> Tree (Node a)
mkTree a = Branch None (Node a 1 1 1) None

incCounter :: Node a -> Node a
incCounter node = Node (nelem node) (nheight node) (ncounter node + 1) (nsize node + 1)

theight :: Tree (Node a) -> Int
theight None = 0
theight (Branch _ node _) = nheight node

tsize :: Tree (Node a) -> Int
tsize None = 0
tsize (Branch _ node _) = nsize node

zfixheight :: TreeZ (Node a) -> TreeZ (Node a)
zfixheight (node, Context l r list) = (fixed, Context l r list)
  where lh = theight l
        rh = theight r
        ls = tsize l
        rs = tsize r
        fixed = Node (nelem node) 
                     (1 + max lh rh) 
                     (ncounter node) 
                     (ls + rs + ncounter node)

tfixheight :: Tree (Node a) -> Tree (Node a)
tfixheight t = untz $ zfixheight $ mktz t

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
