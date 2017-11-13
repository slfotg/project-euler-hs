
powerFunction :: Integer -> Integer
powerFunction x = x ^ x

plusLast10 :: Integer -> Integer -> Integer
plusLast10 x y = (x + y) `mod` 10000000000

main :: IO ()
main = putStrLn . show . foldl plusLast10 0 $ map powerFunction [1..1000]
