module Number.Util (
    digits,
    digitSum,
    countDigits,
    factorial,
    digitFactorialSum ) where

import Data.Char (digitToInt)

digits :: Integral a => a -> [Int]
digits = map digitToInt . show . toInteger

digitSum :: Integral a => a -> Int
digitSum = sum . digits

countDigits :: Integral a => a -> Int
countDigits = length . digits

factorial :: Integral a => a -> a
factorial 0 = 1
factorial x = x * (factorial (x - 1))

digitFactorialSum :: Integral a => a -> Int
digitFactorialSum = sum . map factorial . digits
