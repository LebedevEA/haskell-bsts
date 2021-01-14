module Test where

import Test.HUnit
import TestAVL
import Control.DeepSeq (NFData(rnf))
import Data.Time

cortests = TestList [TestLabel "AVL1" (testAVL 512521 1000),
                     TestLabel "AVL2" (testAVL 277189 2000),
                     TestLabel "AVL3" (testAVL 381835 3000)]

main :: IO ()
main = do
  start <- getCurrentTime
  runTestTT cortests
  end <- getCurrentTime
  print $ diffUTCTime end start