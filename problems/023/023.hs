import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set
import Data.List (sort)

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

isAbundant :: Int -> Int -> Bool
isAbundant k a = a > k

abundants :: Int -> [Int]
abundants n = sort
            . Map.keys
            . Map.filterWithKey isAbundant
            $ properDivisorSums n

maxFilter :: Int -> (Int -> Int -> Bool)
maxFilter x = (\a b -> a + b <= x)

getSumOfTwoAbundants :: (Int -> Int -> Bool) -> [Int] -> Set.Set Int
getSumOfTwoAbundants while as = getSumOfTwoAbundants' as Set.empty
    where
        getSumOfTwoAbundants' as@(x:xs) s
            | while x x == False = s
            | otherwise = getSumOfTwoAbundants' xs
                        . Set.union s
                        . Set.fromList
                        . map (+x)
                        $ takeWhile (while x) as
        getSumOfTwoAbundants' [] s = s

abundantSums :: Int -> Set.Set Int
abundantSums n = getSumOfTwoAbundants (maxFilter n) (abundants n)

main :: IO ()
main = putStrLn . show
     . foldl (+) 0
     . Set.difference (Set.fromList [1 .. 28123])
     $ abundantSums 28123
