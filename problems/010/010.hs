
primes :: Integer -> [Integer]
primes max = 2 : sieve [3, 5..max]
    where
        sieve :: [Integer] -> [Integer]
        sieve (p:ns) = p : sieve (minus ns [p * p, p * p + p..max])
        sieve []     = []

minus :: Ord a => [a] -> [a] -> [a]
minus list1@(x:xs) list2@(y:ys) = case (compare x y) of 
    LT -> x : minus  xs  list2
    EQ ->     minus  xs     ys 
    GT ->     minus  list1  ys
minus  xs     _     = xs

main :: IO ()
main = putStrLn . show . sum $ primes 2000000
