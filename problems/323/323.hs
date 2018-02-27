import Data.Ratio
import Numeric
import Number.Util (pascal)

expectedValue' :: Integer -> [[Integer]] -> [Rational] -> [Rational]
expectedValue' _ [] ratios = ratios
expectedValue' factor (p:ps) ratios
    = expectedValue' (factor * 2) ps (ev:ratios)
        where
            ev = (1 + (sum $ zipWith (*) (map toRational $ tail p) (map (+ 1) ratios))) / (toRational (factor - 1))

expectedValue :: Int -> Rational
expectedValue n = head $ expectedValue' 2 (take n $ tail pascal) [0]

main :: IO ()
main = putStrLn $ showGFloat (Just 10) (fromRational $ expectedValue 32) ""
