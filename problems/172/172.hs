import Data.List (group)
import Number.Util (choose, factorial)

possibleSums :: Int -> Int -> [[Int]]
possibleSums n mx = possibleSums' [] [] n mx
    where
        possibleSums' :: [[Int]] -> [Int] -> Int -> Int -> [[Int]]
        possibleSums' list current n' mx'
            |  n' == 0  = current : list
            | mx' == 0  = list
            | mx' == 1  = ((take n' $ repeat 1) ++ current) : list
            | mx'  > n' = possibleSums' list current n' n'
            | otherwise =
                let l = (possibleSums' list (mx' : current) (n' - mx') mx')
                    in possibleSums' l current n' (mx' - 1)

countPermutations :: [Int] -> Integer
countPermutations xs = foldl div (factorial $ sum xs) $ map factorial xs

countCombinations :: Int -> [Int] -> Integer
countCombinations n = fst . foldr f (1, n) . map length . group
    where
        f x (a, b) = (b `choose` x * a, b - x)

countFewRepeated n mx = let xss = filter ((<= 10) . length) $ possibleSums n mx
    in (foldl (+) 0 $ map count xss) * 9 `div` 10
        where
                count xs = (countPermutations xs) * (countCombinations 10 xs)

main :: IO ()
main = putStrLn . show $ countFewRepeated 18 3
