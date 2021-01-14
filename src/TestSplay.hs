module TestSplay where

import TestBST ( testBST )
import BST
import Splay
import Test.HUnit(Test(TestCase),  Assertion )

splay :: Splay Int 
splay = bstempty

testSplay :: Int -> Int -> Test
testSplay n = TestCase . testBST splay n