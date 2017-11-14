import Data.Ratio
import Number.Util (countDigits)

nextIteration :: Integral a => Ratio a -> Ratio a
nextIteration x = recip (x + 1) + 1

numeratorBigger :: Integral a => Ratio a -> Bool
numeratorBigger x = (countDigits $ numerator x) > (countDigits $ denominator x)

main :: IO ()
main = putStrLn . show
     . length
     . filter numeratorBigger
     . take 1000
     . iterate nextIteration $ 3 % 2
