module Digits (
    Digits,
    digits,
    digitSum,
    countDigits ) where

import Data.Char (digitToInt)

class Digits a where
    digits      :: a -> [Int]
    digitSum    :: a -> Int
    countDigits :: a -> Int
    digitSum    = sum    . digits
    countDigits = length . digits

instance Digits Int where
    digits = map digitToInt . show

instance Digits Integer where
    digits = map digitToInt . show
