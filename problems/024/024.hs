import Data.List
import Number.Util (factorial)

nthPermutation :: Eq a => [a] -> Int -> [a]
nthPermutation as n = nthPermutation' (length as) as (n - 1)
    where
        nthPermutation' _    as 0 = as
        nthPermutation' aLen as n =
            h : nthPermutation' (aLen - 1) (delete h as) (n - (fact * d))
                where
                    fact = fromInteger $ factorial (aLen - 1)
                    d    = n `div` fact
                    h    = (as !! d)

main :: IO ()
main = putStrLn $ nthPermutation "0123456789" 1000000
