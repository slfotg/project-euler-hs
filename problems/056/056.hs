import Data.Char (digitToInt)

digits :: Integer -> [Int]
digits = (map digitToInt) . show

digitSum :: Integer -> Int
digitSum = sum . digits

aTob :: [Integer] -> [Integer]
aTob xs = pure (^) <*> xs <*> xs

main :: IO ()
main = putStrLn . show . maximum . map digitSum $ aTob [2..100]
