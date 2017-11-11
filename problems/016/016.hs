import Data.Char

powerDigitSum :: Int -> Int
powerDigitSum e = digitSum $ 2 ^ e

digitSum :: Integer -> Int
digitSum = sum . map digitToInt . show

main :: IO ()
main = putStrLn . show $ powerDigitSum 1000
