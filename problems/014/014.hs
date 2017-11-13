import Data.LookupTree

nextCollatz :: Int -> Int
nextCollatz 1 = 1
nextCollatz x
    | even x    = x `div` 2
    | otherwise = 3 * x + 1

collatzLength :: Int -> Int
collatzLength = (fmap collatzLength' naturals !!!)
    where
        collatzLength' :: Int -> Int
        collatzLength' 0 = 0
        collatzLength' 1 = 1
        collatzLength' n = let next = nextCollatz n in
            if next >= 1000000
                then 1 + collatzLength' next
                else 1 + collatzLength  next

maxStart :: [Int] -> (Int, Int)
maxStart = foldl accum (0, 0)
    where
        accum :: (Int, Int) -> Int -> (Int, Int)
        accum (a, b) i = let c = collatzLength i in
            if c > b
                then (i, c)
                else (a, b)

main :: IO ()
main = putStrLn . show . fst $ maxStart [0..999999]
