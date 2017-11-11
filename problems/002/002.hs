
fib :: [Integer]
fib = fib' 1 1
    where
        fib' :: Integer -> Integer -> [Integer]
        fib' a b = a : fib' b (a + b)

main :: IO()
main = putStrLn . show . sum $ filter even $ takeWhile (<4000000) fib
