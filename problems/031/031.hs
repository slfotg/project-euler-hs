
allCoins = [200, 100, 50, 20, 10, 5, 2, 1]

countWays :: Int -> [Int] -> Int
countWays 0 _      = 1
countWays _ (1:[]) = 1
countWays amount coins@(c:cs)
    | amount < c = countWays amount cs
    | otherwise  = countWays (amount - c) coins + countWays amount cs

main :: IO ()
main = putStrLn . show $ countWays 200 allCoins
