import qualified Data.Set as Set
import Data.Numbers.Primes (primes)
import Number.Util (rotations)

remove :: Ord a => Set.Set a -> [a] -> Set.Set a
remove s = foldr Set.delete s

add :: Ord a => Set.Set a -> [a] -> Set.Set a
add s = foldr Set.insert s

getRotations :: Set.Set Int -> Set.Set Int -> Set.Set Int
getRotations primes r
    | Set.size primes == 0 = r
    | otherwise = if foldl (&&) True $ map (`Set.member` primes) rs
        then getRotations (primes `remove` rs) (r `add` rs)
        else getRotations (primes `remove` rs) r
            where
                rs = rotations $ Set.elemAt 0 primes

circularPrimes :: Int -> Set.Set Int
circularPrimes n =
    getRotations (Set.fromList $ takeWhile (< n) primes) Set.empty

main :: IO ()
main = putStrLn . show . Set.size $ circularPrimes 1000000
