import Data.Char

digits :: Int -> [Int]
digits = (map digitToInt) . show

isDigitFifthPower :: Int -> Bool
isDigitFifthPower x = x == sum (map (^5) (digits x))

main :: IO ()
main = putStrLn . show . sum $ filter isDigitFifthPower [2..200000]
