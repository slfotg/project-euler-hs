
largestPrimeFactor :: Integral a => a -> a
largestPrimeFactor x = largestPrimeFactor' x $ 2 : [3, 5..]
    where
        largestPrimeFactor' :: Integral a => a -> [a] -> a
        largestPrimeFactor' x' factors@(f:fs)
            | x' == f         = x'
            | x' `mod` f == 0 = largestPrimeFactor' (x' `div` f) factors
            | otherwise       = largestPrimeFactor' x' fs

main :: IO()
main = putStrLn . show $ largestPrimeFactor 600851475143
