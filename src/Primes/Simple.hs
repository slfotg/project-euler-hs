module Primes.Simple (
    primes,
    allPrimes,
    primeFactors,
    countFactors,
    allFactors ) where

import Data.List (group)

primes :: Int -> [Int]
primes m = 2 : sieve [3, 5..m]

allPrimes :: [Int]
allPrimes = 2 : sieve [3, 5..]

sieve :: [Int] -> [Int]
sieve (p:ns) = p : sieve (minus ns [p * p, p * p + p..])
sieve []     = []

minus :: Ord a => [a] -> [a] -> [a]
minus list1@(x:xs) list2@(y:ys) = case (compare x y) of
    LT -> x : minus  xs  list2
    EQ ->     minus  xs     ys
    GT ->     minus  list1  ys
minus  xs     _     = xs

primeFactors :: Int -> [Int]
primeFactors = primeFactors' allPrimes

primeFactors' :: [Int] -> Int -> [Int]
primeFactors' primes@(p:ps) n
    | p * p > n      = [n]
    | n `mod` p == 0 = p : primeFactors' primes (n `div` p)
    | otherwise      = primeFactors' ps n

countFactors :: Int -> Int
countFactors = product . map ((+1) . length) . group . primeFactors

allFactors :: Int -> [Int]
allFactors xs = foldr accum [1] . map powers . group $ primeFactors xs
    where
        powers :: [Int] -> [Int]
        powers = scanl (*) 1
        accum xs ys = (*) <$> xs <*> ys
