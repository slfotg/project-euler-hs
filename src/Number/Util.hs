module Number.Util (
    digits,
    digitSum,
    countDigits,
    rotations,
    factorial,
    digitFactorialSum,
    choose,
    pascal ) where

import Data.Char (digitToInt)

digits :: Integral a => a -> [Int]
digits = map digitToInt . show . toInteger

digitSum :: Integral a => a -> Int
digitSum = sum . digits

countDigits :: Integral a => a -> Int
countDigits = length . digits

rotations :: (Read a, Integral a) => a -> [a]
rotations n = take (countDigits n) $ iterate rotate n
    where
        rotate :: (Read a, Integral a) => a -> a
        rotate n = let (d:ds) = show $ toInteger n in
            read (ds ++ [d])

factorial :: Integral a => a -> Integer
factorial 0 = 1
factorial x = (toInteger x) * (factorial (x - 1))

digitFactorialSum :: Integral a => a -> Integer
digitFactorialSum = sum . map factorial . digits

choose :: Integral a => a -> a -> Integer
choose n r = factorial n `div` factorial r `div` factorial (n - r)

pascal :: [[Integer]]
pascal = iterate pascal' [1]
    where
        pascal' xs = 1 : next xs
        next (a:b:cs) = (a + b) : next (b:cs)
        next (_:[])   = [1]
