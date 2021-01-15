module TestAVL (
  testCorAVL,
  testSpeedAVL
) where

import TestBST
import BST
import AVL
import Test.HUnit(Test(TestCase),  Assertion )

avl :: AVL Int 
avl = bstempty

testCorAVL :: Int -> Int -> Test
testCorAVL n = TestCase . testCorBST avl n

testSpeedAVL :: Int -> Int -> Test
testSpeedAVL n = TestCase . testSpeedBST avl n
