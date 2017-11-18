import qualified Data.Map.Lazy as Map

properDivisorSums :: Int -> Map.Map Int Int
properDivisorSums n = foldl updateDivisors initMap [2 .. n]
    where
        initMap :: Map.Map Int Int
        initMap = Map.fromList $ map (\x -> (x, 1 :: Int)) [2 .. n]
        updateDivisors :: Map.Map Int Int -> Int ->  Map.Map Int Int
        updateDivisors m x = foldl updateDivisors' m [x + x, x + 2 * x .. n]
            where
                updateDivisors' :: Map.Map Int Int -> Int ->  Map.Map Int Int
                updateDivisors' m' k = Map.update (\a -> Just (a + x)) k m'

amicableFilter :: Map.Map Int Int -> Int -> Int -> Bool
amicableFilter m k a
    | k > 10000 = False
    | a == 1    = False
    | a == k    = False
    | otherwise = (m Map.! a) == k

amicables :: [Int]
amicables = let divisorSums = properDivisorSums 25320 in
    Map.keys $ Map.filterWithKey (amicableFilter divisorSums) divisorSums

main :: IO ()
main = putStrLn . show $ sum amicables
