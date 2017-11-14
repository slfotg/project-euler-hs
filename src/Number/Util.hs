module Number.Util (
    digits,
    digitSum,
    countDigits,
    factorial ) where

import Data.Char (digitToInt)

digits :: Integral a => a -> [Int]
digits = map digitToInt . show . toInteger

digitSum :: Integral a => a -> Int
digitSum = sum . digits

countDigits :: Integral a => a -> Int
countDigits = length . digits

factorial :: Integral a => a -> Integer
factorial 0 = 1
factorial x = (toInteger x) * (factorial (x - 1))
