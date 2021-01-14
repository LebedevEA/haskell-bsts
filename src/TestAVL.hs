module TestAVL where

import TestBST ( testBST )
import BST
import AVL
import Test.HUnit( Assertion )

avl :: AVL Int 
avl = bstempty

testAVL :: Int -> Int -> Assertion
testAVL = testBST avl
-- 15 2 does not work