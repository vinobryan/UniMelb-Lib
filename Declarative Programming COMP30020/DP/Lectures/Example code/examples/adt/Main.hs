-- vim: ts=4 sw=4 expandtab syntax=haskell

module Main (main) where

import Coord

main = do
    let p1 = mkpoint 1.0 2.0
    let p2 = Cartesian 3.0 4.0
    putStrLn $ show ((getx p2) - (getx p1))
