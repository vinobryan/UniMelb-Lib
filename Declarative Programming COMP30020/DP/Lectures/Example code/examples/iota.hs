-- DON'T USE THIS DEFINITION as the (n+k) pattern form is no longer 
-- supported as of GHC 7

module Iota where

iota :: Int -> [Int]
iota 0     = []
iota (n+1) = iota n ++ [n+1]
