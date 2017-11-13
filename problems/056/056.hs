import Digits (digitSum)

aTob :: [Integer] -> [Integer]
aTob xs = pure (^) <*> xs <*> xs

main :: IO ()
main = putStrLn . show . maximum . map digitSum $ aTob [2..100]
