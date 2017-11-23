import Data.Numbers.Primes (isPrime)

quadratic :: (Integer, Integer) -> (Integer -> Integer)
quadratic (a, b) = (\n -> n * n + a * n + b)

toTuple :: a -> a -> (a, a)
toTuple a b = (a, b)

pairs :: [(Integer, Integer)]
pairs = map toTuple [-999 .. 999] <*> [-1000 .. 1000]

getPrimesLength :: (Integer, Integer) -> Int
getPrimesLength p = length . takeWhile isPrime $ map (quadratic p) [0..]

main = putStrLn . show . product' . fst $ foldl f ((0, 0), 0) pairs
    where
        f a x = let b = (x, getPrimesLength x) in
            if (snd b) > (snd a) then b else a
        product' (a, b) = a * b
