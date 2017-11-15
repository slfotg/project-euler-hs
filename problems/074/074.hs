import Prelude hiding (filter)
import Data.Map.Lazy (
        Map, (!), insert, union, empty, member, update, filter, size)
import Data.Char (digitToInt)
import Data.List (foldl')
import Number.Util (digitFactorialSum)

type Lookup = Map Int Int

-- 69 → 363600 → 1454 → 169 → 363601 (→ 1454)
-- [(69, 0)]
-- [(69, 0), (363600, 1)]
-- [(69, 0), (363600, 1), (1454, 2)]
-- [(69, 0), (363600, 1), (1454, 2), (169, 3)]
-- [(69, 0), (363600, 1), (1454, 2), (169, 3), (363601, 4)]
--
-- [(69, 5), (363600, 4), (1454, 3), (169, 3), (363601, 3), (1454, 5)]
getChainLengths :: Lookup -> Int -> Lookup
getChainLengths m n = m `union` getChainLengths' m empty n 0
    where
        getChainLengths' :: Lookup -> Lookup -> Int -> Int -> Lookup
        getChainLengths' m c n ix =
            if n `member` m
                then fmap (update 0 $ (m ! n) + ix) c
                else if n `member` c
                    then fmap (update (ix - (c ! n)) (c ! n)) c
                    else getChainLengths' m (insert n ix c)
                                          (digitFactorialSum n) (ix + 1)
        update :: Int -> Int -> Int -> Int
        update cycleLength x n =
            if (n < x)
                then (x - n + cycleLength)
                else cycleLength

buildChainLengths :: Int -> Lookup
buildChainLengths x = foldl' getChainLengths empty [0..x]

main :: IO ()
main = putStrLn . show . size . filter (== 60) $ buildChainLengths 1000000
