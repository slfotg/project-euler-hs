import Data.List

triangles :: [Int]
triangles = scanl (+) 1 [2..]

primes :: [Int]
primes = 2 : sieve [3, 5..]
    where
        sieve :: [Int] -> [Int]
        sieve (p:ns) = p : sieve (minus ns [p * p, p * p + p..])
        sieve []     = []

minus :: Ord a => [a] -> [a] -> [a]
minus list1@(x:xs) list2@(y:ys) = case (compare x y) of 
    LT -> x : minus  xs  list2
    EQ ->     minus  xs     ys 
    GT ->     minus  list1  ys
minus  xs     _     = xs

primeFactors :: Int -> [Int]
primeFactors n = primeFactors' n primes
    where
        primeFactors' :: Int -> [Int] -> [Int]
        primeFactors' n' primes'@(p:ps)
            | p * p > n'      = [n']
            | n' `mod` p == 0 = p : primeFactors' (n' `div` p) primes'
            | otherwise       = primeFactors' n' ps

countFactors :: Int -> Int
countFactors = product . map ((+1) . length) . group . primeFactors

highlyDivisibleTriangle :: Int -> Int
highlyDivisibleTriangle x = findTriangle triangles
    where
        findTriangle (t:ts)
            | countFactors t > x = t
            | otherwise          = findTriangle ts

main :: IO ()
main = putStrLn . show $ highlyDivisibleTriangle 500
