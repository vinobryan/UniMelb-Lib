--  File     : publictests.hs
--  Author   : Peter Schachte
--  Purpose  : test cases for Assignment1 project

import Assignment1
import HaskellTest

suite = 
  TimeLimit 2.0 $
  Suite [
    expect (elementPosition 3 [1,2,3,4,5]) (2),
    expect (everyNth 4 "elephant") ("pt"),
    expect (elementBefore 3 [1,2,3,4,5]) (Just 2)
    ]

main :: IO ()
main = do
  testVerbose suite
