import qualified Data.Map.Lazy as Map
import Data.List (sort, group)
import Number.Util (digits, countPermutations)

type Digits = [Int]

initMap :: Map.Map Digits Int
initMap = Map.fromList [([0], 0), ([1], 1), ([8, 9], 89)]

digitSquareSum :: Digits -> Digits
digitSquareSum = sort . digits . sum . map (^2)

tupleSwap :: b -> a -> (a, b)
tupleSwap b a = (a, b)

allDigits :: Int -> [Digits]
allDigits x = allDigits' 9 0 []
    where
        allDigits' :: Int -> Int -> Digits -> [Digits]
        allDigits' n l ds
            | l == x    = [ds]
            | n > 0     = allDigits'  n     (l + 1) next
                       ++ allDigits' (n - 1) l      ds
            | otherwise = allDigits'  n     (l + 1) next
                where
                    next = n : ds

createChainMap :: [Digits] -> Map.Map Digits Int
createChainMap = foldr updateMap initMap
    where
        updateMap :: Digits -> Map.Map Digits Int -> Map.Map Digits Int
        updateMap digits m
            | Map.member digits m = m
            | otherwise = updateChain digits m []
        updateChain :: Digits -> Map.Map Digits Int -> [Digits] -> Map.Map Digits Int
        updateChain digits m a
            | Map.member digits m = Map.union m
                                  . Map.fromList
                                  $ map (tupleSwap $ m Map.! digits) a
            | otherwise = updateChain (digitSquareSum digits) m $ digits : a

main :: IO ()
main = print
     . sum
     . map countPermutations
     . filter ((==) 7 . length)
     . Map.keys
     . Map.filter (== 89)
     . createChainMap
     $ allDigits 7
