module Number.Util (
    digits,
    digitSum,
    countDigits,
    rotations,
    factorial,
    digitFactorialSum ) where

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

factorial :: Integral a => a -> a
factorial 0 = 1
factorial x = x * (factorial (x - 1))

digitFactorialSum :: Integral a => a -> Int
digitFactorialSum = sum . map factorial . digits
