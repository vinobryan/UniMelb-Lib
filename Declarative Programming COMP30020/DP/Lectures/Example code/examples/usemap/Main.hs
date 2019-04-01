-- vim: ts=4 sw=4 expandtab syntax=haskell

module Main where

import Data.Map
import Data.Set

m = Data.Map.singleton 1 "one"
s = Data.Set.singleton 1

main :: IO ()
main = do
    putStrLn (show m)
    putStrLn (show s)
