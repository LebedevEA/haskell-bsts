module TestSplay (
  testCorSplay,
  testSpeedSplay
) where

import TestBST
import BST
import Splay
import Test.HUnit( Test(TestCase),  Assertion )

splay :: Splay Int 
splay = bstempty

testCorSplay :: Int -> Int -> Test
testCorSplay n = TestCase . testCorBST splay n

testSpeedSplay :: Int -> Int -> Test
testSpeedSplay n = TestCase . testSpeedBST splay n