
factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * factorial (x - 1)

countRoutes :: Integer -> Integer
countRoutes x = let d = factorial x in div (factorial (2 * x)) (d * d)

main :: IO ()
main = putStrLn . show $ countRoutes 20
