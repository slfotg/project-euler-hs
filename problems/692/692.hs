
fibonacci :: [Integer]
fibonacci = 1 : 2 : zipWith (+) fibonacci (tail fibonacci)

sums :: [Integer]
sums = 1 : 3 : zipWith (+) (tail fibonacci) (zipWith (+) sums (tail sums))

findG :: Integer -> Integer
findG n = findG' n fibonacci sums
    where
        findG' :: Integer -> [Integer] -> [Integer] -> Integer
        findG' n (f:fs) (g:gs)
            | f == n = g
            | otherwise = findG' n fs gs

main :: IO ()
main = print $ findG 23416728348467685