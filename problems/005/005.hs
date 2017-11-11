
primes :: [Integer]
primes = [2, 3, 5, 7, 11, 13, 17, 19]

largestFactor :: Integer -> Integer -> Integer
largestFactor max n = largestFactor' n
    where
        largestFactor' :: Integer -> Integer
        largestFactor' current = let next = current * n in
            if current * n > max
                then current
                else largestFactor' $ current * n

main :: IO()
main = putStrLn . show . product $ map (largestFactor 20) primes
