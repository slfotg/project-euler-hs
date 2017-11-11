import Data.Char

digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

main :: IO ()
main = putStrLn . show . digitSum $ factorial 100
