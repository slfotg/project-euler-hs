
sumOfSquares :: [Integer] -> Integer
sumOfSquares = sum . map (^2)

squareOfSum :: [Integer] -> Integer
squareOfSum = (^2) . sum

difference :: [Integer] -> Integer
difference xs = (squareOfSum xs) - (sumOfSquares xs)

main :: IO()
main = putStrLn . show $ difference [1..100]
