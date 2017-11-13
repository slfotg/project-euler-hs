import Data.Char

digits :: Int -> [Int]
digits = (map digitToInt) . show

factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)

isSumOfFactorial :: Int -> Bool
isSumOfFactorial x = (sum $ map factorial (digits x)) == x

main :: IO ()
main = putStrLn . show . sum $ filter isSumOfFactorial [3..100000]
