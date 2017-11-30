import Data.Numbers.Primes

maxN = 10 ^ 10

getSolution m = getSolution' 1 primes
    where
        getSolution' n (p:ps) =
            let x = ((p - 1) ^ n + (p + 1) ^ n) `mod` (p^2) in
                if x > m
                    then n
                    else getSolution' (n + 1) ps

main :: IO ()
main = print $ getSolution maxN
