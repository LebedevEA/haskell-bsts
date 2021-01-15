{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NumericUnderscores #-}

module Main where

import Test.HUnit ( runTestTT, Test(TestLabel, TestList) )
import TestBST
import Control.DeepSeq ( NFData(rnf) )
import Data.Time ( diffUTCTime, getCurrentTime )
import Numeric

cortests :: Test
cortests = TestList [TestLabel "AVL Correctness #1" (testCorAVL 512521 1000),
                     TestLabel "AVL Correctness #2" (testCorAVL 277189 2000),
                     TestLabel "AVL Correctness #3" (testCorAVL 381835 3000),
                     TestLabel "Splay Correctness #1" (testCorSplay 124144 1000),
                     TestLabel "Splay Correctness #2" (testCorSplay 417851 2000),
                     TestLabel "Splay Correctness #3" (testCorSplay 991015 3000),
                     TestLabel "Treap Correctness #1" (testCorTreap 148184 1000),
                     TestLabel "Treap Correctness #2" (testCorTreap 588142 2000),
                     TestLabel "Treap Correctness #3" (testCorTreap 100031 3000)]

speedBST3xk :: Int ->  (Int -> Int -> Test) -> String -> Test
speedBST3xk x bt name = TestList [TestLabel (name ++ " Speed " ++ (show x) ++ " #1") (bt 23572 x),
                                  TestLabel (name ++ " Speed " ++ (show x) ++ " #2") (bt 18241 x),
                                  TestLabel (name ++ " Speed " ++ (show x) ++ " #3") (bt 10479 x)]
                                  

speedAVL3x k = speedBST3xk k testSpeedAVL "AVL"
speedSplay3x k = speedBST3xk k testSpeedSplay "Splay"
speedTreap3x k = speedBST3xk k testSpeedTreap "Treap"
speedNaive3x k = speedBST3xk k testSpeedNaive "Naive"

speedTest :: Test -> IO Double
speedTest f = do
  start <- getCurrentTime
  runTestTT f
  end <- getCurrentTime
  return $ realToFrac $ diffUTCTime end start

main :: IO ()
main = do
  runTestTT cortests

  avl20 <- speedTest $ speedAVL3x 20_000
  splay20 <- speedTest $ speedSplay3x 20_000
  treap20 <- speedTest $ speedTreap3x 20_000
  naive20 <- speedTest $ speedNaive3x 20_000

  avl100 <- speedTest $ speedAVL3x 100_000
  splay100 <- speedTest $ speedSplay3x 100_000
  treap100 <- speedTest $ speedTreap3x 100_000

  let avl100s = (showGFloat (Just 2) $ avl100) ""
  let avl20s = (showGFloat (Just 2) $ avl20) ""
  let splay100s = (showGFloat (Just 2) $ splay100) ""
  let splay20s = (showGFloat (Just 2) $ splay20) ""
  let treap100s = (showGFloat (Just 2) $ treap100) ""
  let treap20s = (showGFloat (Just 2) $ treap20) ""
  let naive20s = (showGFloat (Just 2) $ naive20) ""

  putStrLn ""
  putStrLn   "BST   | 3x10k | 3x100k"
  putStrLn $ "Naive | " ++ naive20s ++ "s | ..."
  putStrLn $ "AVL   | " ++ avl20s   ++ "s | " ++ avl100s ++ "s"
  putStrLn $ "Splay | " ++ splay20s ++ "s | " ++ splay100s ++ "s"
  putStrLn $ "Treap | " ++ treap20s ++ "s | " ++ treap100s ++ "s"