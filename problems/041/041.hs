import Data.List (permutations)
import Data.Numbers.Primes (isPrime)

toInt :: [Int] -> Int
toInt = foldl ((+) . (*10)) 0

main :: IO ()
main = putStrLn . show
     . maximum
     . filter isPrime
     . map toInt
     $ permutations [1..7]
