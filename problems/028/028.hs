
diagonalSum :: Int -> Int
diagonalSum x = 1 + diagonalSum' 0 1 2
    where
        diagonalSum' i n d
            | d > x  = 0
            | i == 4 = diagonalSum' 0 n (d + 2)
            | otherwise = let next =  n + d in
                next + diagonalSum' (i + 1) next d

main :: IO ()
main = putStrLn . show $ diagonalSum 1001
