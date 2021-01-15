module TestTreap (
  testCorTreap,
  testSpeedTreap
) where

import TestBST
import BST
import Treap
import Test.HUnit(Test(TestCase),  Assertion )

treap :: Treap Int 
treap = bstempty

testCorTreap :: Int -> Int -> Test
testCorTreap n = TestCase . testCorBST treap n

testSpeedTreap :: Int -> Int -> Test
testSpeedTreap n = TestCase . testSpeedBST treap n
