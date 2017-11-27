import Data.List (sort)

isPermuted :: Integer -> Bool
isPermuted n = foldr (&&) True
             $ map ((== (sort . show) n) . sort . show . (*n)) [2 .. 6]

main :: IO ()
main = putStrLn . show $ filter isPermuted [1 ..] !! 0
