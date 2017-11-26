import Data.Ratio

type Fraction = (Integer, Integer)

equals :: Fraction -> Fraction -> Bool
equals a b = (toRatio a) == (toRatio b)

cancelDigits :: Fraction -> Fraction
cancelDigits (a, b) = (a `div` 10, b `mod` 10)

toRatio :: Fraction -> Ratio Integer
toRatio (a, b) = a % b

denominators :: [Integer]
denominators = filter (\x -> x `mod` 10 /= 0) [11 .. 99]

numerators :: Integer -> [Integer]
numerators d = let m = d `div` 10 in [m + 10, m + 20 .. (d - 1)]

fractions :: [Fraction]
fractions = foldr (++) [] $ map fractions' denominators
    where
        fractions' d = map (\n -> (n, d)) $ numerators d

isCurious :: Fraction -> Bool
isCurious f = f `equals` (cancelDigits f)

main :: IO ()
main = putStrLn . show
     . denominator
     . product
     . map toRatio
     $ filter isCurious fractions
