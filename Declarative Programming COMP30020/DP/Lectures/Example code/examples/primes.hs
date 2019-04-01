-- vim: ts=4 sw=4 expandtab syntax=haskell

module Primes where

-- [2..] is equivalent to all_ints_from 2
all_ints_from :: Integer -> [Integer]
all_ints_from n = n:(all_ints_from (n+1))

all_primes :: [Integer]
all_primes = prime_filter ([2..])

prime_filter :: [Integer] -> [Integer]
prime_filter [] = []
prime_filter (x:xs) = x:(prime_filter (filter_multiples x xs))

-- filter n xs: filter out multiples of n from xs
filter_multiples :: Integer -> [Integer] -> [Integer]
filter_multiples _ [] = []
filter_multiples n (x:xs)
    | (mod x n) == 0 = filter_multiples n xs
    | otherwise      = x:(filter_multiples n xs)

-- The first n primes:
--    take n all_primes

-- All the primes up to n:
--    takeWhile (<= n) all_primes