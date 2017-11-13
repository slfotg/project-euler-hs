import Numeric (showIntAtBase)
import Data.Char (intToDigit)

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

isBase10Palindrome :: Int -> Bool
isBase10Palindrome = isPalindrome . show

isBase2Palindrome :: Int -> Bool
isBase2Palindrome x = isPalindrome $ showIntAtBase 2 intToDigit x ""

main :: IO ()
main = putStrLn . show . sum . filter isBase2Palindrome $ filter isBase10Palindrome [1, 3..1000000]
