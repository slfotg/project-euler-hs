import Number.Util (pascal)
import Data.Ratio
-- [1]
-- [1,1]
-- [1,2,3]
-- [1,3,9,11]
-- [1,4,18,44,53]
-- [1,5,30,110,265,309]
-- [1,6,45,220,795,1854,2119]
-- [1,7,63,385,1855,6489,14833,16687]
getCounts :: [[Integer]]
getCounts = getCounts' 1 1 pascal []
    where
        getCounts' :: Integer -> Integer -> [[Integer]] -> [Integer] -> [[Integer]]
        getCounts' fact n (p:ps) accum = (next : headList) : getCounts' fact' (n + 1) ps (next : accum)
            where
                fact' = fact * n
                headList = zipWith (*) accum $ tail p
                next = fact' - (sum headList)

expectedValue :: Integer -> Rational
expectedValue 1 = 0
expectedValue n = expectedValue' 2 2 (tail getCounts) [0]
    where
        expectedValue' :: Integer -> Integer -> [[Integer]] -> [Rational] -> Rational
        expectedValue' fact n' ((a:bs):cs) rs
            | n == n' = (subListSum + (toRational a * ev)) / toRational fact
            | otherwise = expectedValue' (fact * nextN) nextN cs (ev:rs)
                where
                    nextN = n' + 1
                    subListSum :: Rational
                    subListSum = sum . zipWith (*) rs $ map toRational bs
                    ev = (subListSum + (toRational fact)) / toRational (fact - a)

roundFractional n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

reduce :: [Int] -> [Int]
reduce xs = reduce' [] xs
    where
        reduce' :: [Int] -> [Int] -> [Int]
        reduce' accum (a:xs@(b:_))
            | a + 1 == b = reduce' (map (subOne a) accum) (map (subOne a) xs)
            | otherwise  = reduce' (accum ++ [a]) xs
        reduce' accum xs = accum ++ xs
        subOne :: Int -> Int -> Int
        subOne n x
            | x < n = x
            | otherwise = x - 1

main = print . roundFractional 8 . fromRational $ expectedValue 52
