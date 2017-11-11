
-- TODO Get a better sieve
primes :: [Integer]
primes = 2 : sieve [3, 5..]
    where
        sieve (p:xs) = p : sieve (filter (\x -> x `mod` p /= 0) xs)

main :: IO()
main = putStrLn . show $ primes !! 10000
