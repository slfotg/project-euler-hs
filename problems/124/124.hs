import qualified Data.Map.Lazy as Map
import Data.List (sort)
import Data.Tuple (swap)
import Data.Numbers.Primes

maxN = 10

initMap :: Integer -> Map.Map Integer Integer
initMap n = Map.fromList $ map (\x -> (x, 1)) [1 .. n]

updateMap :: Integer
          -> Integer
          -> Map.Map Integer Integer
          -> Map.Map Integer Integer
updateMap n p mp = foldr updateMap' mp [p, p + p .. n]
    where
        updateMap' = Map.updateWithKey (\k a -> Just $ a * p)

getSortedList n = sort
                . map swap
                . Map.toList
                . foldr (updateMap n) (initMap n)
                $ takeWhile (<= n) primes

main :: IO ()
main = print . snd $ getSortedList 100000 !! 9999
