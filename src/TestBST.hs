{-# LANGUAGE ScopedTypeVariables #-}
module TestBST ( testBST ) where

import BST ( BST(kthelem, add, size) )

import Test.HUnit (assertEqual, Assertion, assertBool)
import System.Random (mkStdGen, StdGen, randomR, Random, random)
import Data.List (sort)

testBST :: (BST m) => m Int -> Int -> Int -> Assertion
testBST bst seed iters = rt iters (mkStdGen seed) bst []

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
                    (kthelem (k + 0) bst) 
                    (sort xs !! k)
        rt (it - 1) rnd'' bst xs
      else do
        rt (it - 1) rnd' bst xs
      
