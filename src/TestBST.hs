{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module TestBST ( 
  testCorAVL,
  testSpeedAVL,
  testCorSplay,
  testSpeedSplay,
  testCorTreap,
  testSpeedTreap,
  testCorNaive,
  testSpeedNaive 
) where

import BST ( BST(..) )

import Test.HUnit (assertEqual, Assertion, assertBool, Test(..) )
import System.Random (mkStdGen, StdGen, randomR, Random, random)
import Data.List (sort)
import Control.Monad (guard)
import AVL
import Splay
import Treap
import Naive

testCorBST :: (BST m) => m Int -> Int -> Int -> Assertion
testCorBST bst seed iters = rt iters (mkStdGen seed) bst []

testSpeedBST :: (BST m) => m Int -> Int -> Int -> Assertion
testSpeedBST bst seed iters = st iters (mkStdGen seed) bst

data Scenario = Add | Kth

instance Random Scenario where
  randomR _ = random 
  random gen = 
    let (p::Int, gen') = randomR (0,1) gen
    in case p of
         0 -> (Add, gen')
         1 -> (Kth, gen')

rt :: (BST m) => Int -> StdGen -> m Int -> [Int] -> Assertion 
rt it rnd bst xs | it == 0 = return () 
                 | otherwise = do
  let (scenario::Scenario, rnd') = random rnd
  case scenario of
    Add -> do 
      let (x::Int, rnd'') = random rnd'
      rt (it - 1) rnd'' (add bst x) (x:xs)
    Kth -> do
      assertEqual "size of bst is not equal to lenths of list" 
                  (length xs) (size bst) 
      if size bst >= 1 then do
        let (k::Int, rnd'') = randomR (0, size bst - 1) rnd'
        assertEqual (show k ++ "th elem in list does not match" 
                      ++ show k ++ "th elem in bst") 
                    (kthelem k bst) 
                    (sort xs !! k)
        rt (it - 1) rnd'' bst xs
      else do
        rt (it - 1) rnd' bst xs

st :: (BST m) => Int -> StdGen -> m Int -> Assertion
st it rnd bst | it == 0 = return ()
              | otherwise = do
  let (scenario::Scenario, rnd') = random rnd
  case scenario of
    Add -> do
      let (x::Int,rnd'') = random rnd'
      st (it - 1) rnd'' (add bst x)
    Kth -> do
      guard $ size bst > 0
      let (k::Int, rnd'') = randomR (0, size bst - 1) rnd'
      let !r = kthelem k bst
      st (it - 1) rnd'' bst

avl :: AVL Int 
avl = bstempty

testCorAVL :: Int -> Int -> Test
testCorAVL n = TestCase . testCorBST avl n

testSpeedAVL :: Int -> Int -> Test
testSpeedAVL n = TestCase . testSpeedBST avl n

splay :: Splay Int 
splay = bstempty

testCorSplay :: Int -> Int -> Test
testCorSplay n = TestCase . testCorBST splay n

testSpeedSplay :: Int -> Int -> Test
testSpeedSplay n = TestCase . testSpeedBST splay n

treap :: Treap Int 
treap = bstempty

testCorTreap :: Int -> Int -> Test
testCorTreap n = TestCase . testCorBST treap n

testSpeedTreap :: Int -> Int -> Test
testSpeedTreap n = TestCase . testSpeedBST treap n

naive :: Naive Int
naive = bstempty

testCorNaive :: Int -> Int -> Test
testCorNaive n = TestCase . testSpeedBST naive n

testSpeedNaive :: Int -> Int -> Test
testSpeedNaive n = TestCase . testSpeedBST naive n
