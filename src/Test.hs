module Test where

import Test.HUnit ( runTestTT, Test(TestLabel, TestList) )
import TestAVL ( testAVL )
import TestSplay ( testSplay )
import Control.DeepSeq (NFData(rnf))
import Data.Time ( diffUTCTime, getCurrentTime )

cortests :: Test
cortests = TestList [TestLabel "AVL1" (testAVL 512521 1000),
                     TestLabel "AVL2" (testAVL 277189 2000),
                     TestLabel "AVL3" (testAVL 381835 3000),
                     TestLabel "Splay1" (testSplay 124144 1000),
                     TestLabel "Splay2" (testSplay 417851 2000),
                     TestLabel "Splay3" (testSplay 991015 3000)]

main :: IO ()
main = do
  start <- getCurrentTime
  runTestTT cortests
  end <- getCurrentTime
  print $ diffUTCTime end start