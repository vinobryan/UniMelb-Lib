-- vim: ts=4 sw=4 expandtab syntax=haskell

module Main where

import Data.Set -- (singleton)
import Data.Map -- (empty, insert)

s = singleton 1
m = insert 1 "one" empty

main :: IO ()
main = do
    putStrLn (show s)
    putStrLn (show m)
