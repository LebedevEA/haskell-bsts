module TestTreap where

import TestBST ( testBST )
import BST
import Treap
import Test.HUnit(Test(TestCase),  Assertion )

treap :: Treap Int 
treap = bstempty

testTreap :: Int -> Int -> Test
testTreap n = TestCase . testBST treap n
