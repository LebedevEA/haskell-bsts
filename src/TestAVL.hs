module TestAVL where

import TestBST ( testBST )
import BST
import AVL
import Test.HUnit(Test(TestCase),  Assertion )

avl :: AVL Int 
avl = bstempty

testAVL :: Int -> Int -> Test
testAVL n = TestCase . testBST avl n
