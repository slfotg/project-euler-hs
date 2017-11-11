
tripleProduct :: (Int, Int, Int) -> Int
tripleProduct (a, b, c) = a * b * c

getTriplet :: (Int, Int) -> (Int, Int, Int)
getTriplet (m, n) = (m^2 - n^2, 2 * m * n, m^2 + n^2)

findTriple :: Int -> (Int, Int, Int)
findTriple x = getTriplet $ findMAndN (x `div` 2) [2..]
    where
        findMAndN :: Int -> [Int] -> (Int, Int)
        findMAndN x' (m:ms) = let d = x' - (m * m) in
            if d `mod` m == 0
                then let n = d `div` m in
                    if n < m
                        then (m, n)
                        else findMAndN x' ms
                else findMAndN x' ms

main :: IO ()
main = putStrLn . show . tripleProduct $ findTriple 1000
