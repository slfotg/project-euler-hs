import qualified Data.Map.Lazy as Map

getDecimalCycleLength :: Integer -> Int
getDecimalCycleLength n = getCycleLength 10 1 Map.empty
    where
        getCycleLength :: Integer -> Int -> Map.Map Integer Int -> Int
        getCycleLength x i m
            | x == 0         = 0
            | Map.member x m = i - (m Map.! x)
            | otherwise      =
                if q == 0
                    then getCycleLength (x * 10) (i + 1) m'
                    else getCycleLength (r * 10) (i + 1) m'
                where
                    r  = x `mod` n
                    q  = x `div` n
                    m' = Map.insert x i m

maxPair :: (Integer, Int) -> (Integer, Int) -> (Integer, Int)
maxPair p1@(_, y1) p2@(_, y2)
    | y1 > y2   = p1
    | otherwise = p2

main :: IO ()
main = putStrLn . show
     . fst
     . foldl maxPair (0, 0)
     $ map (\x -> (x, getDecimalCycleLength x)) [1 .. 999]
