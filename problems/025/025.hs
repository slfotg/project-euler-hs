
fib :: [Integer]
fib = fib' 0 1
    where
        fib' :: Integer -> Integer -> [Integer]
        fib' a b = a : fib' b (a + b)

thousandDigits = 10 ^ 999

getIndex :: Integer -> [Integer] -> Int -> Int
getIndex max (x:xs) currentIndex
    | x > max = currentIndex
    | otherwise = getIndex max xs $ currentIndex + 1

main :: IO ()
main = putStrLn . show $ getIndex thousandDigits fib 0
