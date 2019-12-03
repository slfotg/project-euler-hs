module PE.Primes (
    PrimeFactors,
    Factoring,
    factors,
    primes,
    primeFactors,
    distinctPrimeFactors,
    primeFactorExponents,
    primeExponentPairs ) where

import qualified Data.Numbers.Primes as Primes
import Data.List (group, intercalate)

class Factoring a where
    factors :: a -> [a]

-- PrimePowerPair definition
data PrimeFactors = PFs Integer [(Integer, Integer)]

primes = Primes.primes

fromPrime :: Integral a => a -> PrimeFactors
fromPrime p = let p' = toInteger p in PFs p' [(p', 1)]

distinctPrimeFactors :: PrimeFactors -> [Integer]
distinctPrimeFactors = map fst . primeExponentPairs

primeFactorExponents :: PrimeFactors -> [Integer]
primeFactorExponents = map snd . primeExponentPairs

primeExponentPairs :: PrimeFactors -> [(Integer, Integer)]
primeExponentPairs (PFs _ pes) = pes

primeFactors :: Integral a => a -> PrimeFactors
primeFactors 0 = PFs 0 []
primeFactors n = PFs (toInteger n) . primeFactors' $ abs n
    where
        primeFactors' = map primeExponent . group . Primes.primeFactors
        primeExponent xs = (toInteger $ head xs, toInteger $ length xs)

instance Show PrimeFactors where
    show (PFs n ps)
        | n == 0 = "0"
        | n == 1 = "1"
        | n == (-1) = "-1"
        | n <  0 = show n ++ " = -(" ++ (intercalate " * " $ map showTuple ps) ++ ")"
        | otherwise = show n ++ " = " ++ (intercalate " * " $ map showTuple ps)
            where
                showFactors ps = (intercalate " * " $ map showTuple ps)
                showTuple (p, 1) = show p
                showTuple (p, e) = show p ++ " ^ " ++ show e

instance Eq PrimeFactors where
    (==) (PFs a _) (PFs b _) = a == b

instance Ord PrimeFactors where
    compare (PFs a _) (PFs b _) = compare a b

instance Num PrimeFactors where
    (+) (PFs n _) (PFs m _) = primeFactors $ n + m -- TODO maybe use gcd and stuff
    (*) (PFs n ps) (PFs m qs) = PFs (n * m) $ mul ps qs
        where
            mul xs@((p,e):ps) ys@((q,f):qs)
                | p == q = (p, e + f) : mul ps qs
                | p <  q = (p, e) : mul ps ys
                | otherwise = (q, f) : mul xs qs
            mul [] ys = ys
            mul xs [] = xs
    negate (PFs n ps) = PFs (-n) ps
    abs (PFs n ps) = PFs (abs n) ps
    signum (PFs n _) = PFs (signum n) []
    fromInteger = primeFactors

instance Enum PrimeFactors where
    toEnum = primeFactors
    fromEnum (PFs n _) = fromEnum n

instance Real PrimeFactors where
    toRational (PFs n _) = toRational n

instance Integral PrimeFactors where
    quotRem (PFs n ps) (PFs m qs) =
        (primeFactors $ quot n m, primeFactors $ rem n m)
    toInteger (PFs n _) = n

instance Semigroup PrimeFactors where
    (<>) = (*)

instance Monoid PrimeFactors where
    mempty = PFs 1 []

instance Factoring PrimeFactors where
    factors (PFs _ pes) = foldr accum [mempty] $ map tupleToPfs pes
        where
            accum xs ys = (*) <$> xs <*> ys
            tupleToPfs (p, e) = take (fromInteger $ e + 1)
                              $ iterate (* (fromPrime p)) mempty

instance Factoring Integer where
    factors = map toInteger . factors . primeFactors

instance Factoring Int where
    factors = map fromEnum . factors . primeFactors
