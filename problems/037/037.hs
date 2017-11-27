import qualified Data.Set as Set
import Data.Numbers.Primes

firstFour = Set.fromList [2, 3, 5, 7]

truncatablePrimes :: [Integer]
truncatablePrimes = truncatablePrimes' (drop 4 primes) firstFour firstFour
    where
        truncatablePrimes' (p:ps) lefts rights =
            if isLeftTruncatable lefts p
                then if isRightTruncatable rights p
                    then p : truncatablePrimes' ps (p `Set.insert` lefts) (p `Set.insert` rights)
                    else truncatablePrimes' ps (p `Set.insert` lefts) rights
                else if isRightTruncatable rights p
                    then truncatablePrimes' ps lefts (p `Set.insert` rights)
                    else truncatablePrimes' ps lefts rights

isLeftTruncatable :: Set.Set Integer -> Integer -> Bool
isLeftTruncatable lefts n = (read . tail $ show n) `Set.member` lefts

isRightTruncatable :: Set.Set Integer -> Integer -> Bool
isRightTruncatable rights n = (n `div` 10) `Set.member` rights

main :: IO ()
main = putStrLn . show . sum $ take 11 truncatablePrimes
