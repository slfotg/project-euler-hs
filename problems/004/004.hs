import Data.List

isPalindrome :: Integer -> Bool
isPalindrome x = let s = show x in reverse s == s

palindromeProducts :: Integer -> Integer -> [Integer]
palindromeProducts a z = filter isPalindrome allProducts
    where
        allProducts :: [Integer]
        allProducts = intercalate [] $ map (\x -> map (*x) [x..z]) [a..z]

main :: IO()
main = putStrLn . show . maximum $ palindromeProducts 100 999
