import qualified Data.Map.Strict as Map

type LookupMap = Map.Map (Int, Int) Int

countSums :: Int -> Int
countSums x = countSums' Map.empty x (x - 1) --sum $ map (countSums' x) [1 .. (x - 1)]
--
-- f(5)
-- f'(5, 4) = 4, 1
countSums' :: LookupMap -> Int -> Int -> Int
countSums' _ 0 _ = 1
countSums' _ _ 1 = 1
countSums' lookupMap x d
    | x < d = countSums' lookupMap x x
    | (x, d) `Map.member` lookupMap = lookupMap Map.! (x, d)
    | otherwise =
        let count = countSums' lookupMap (x - d) d
                  + countSums' lookupMap x (d - 1)
            in Map.insert (x, d) count lookupMap Map.! (x, d)

main :: IO ()
main = putStrLn . show $ countSums 100
