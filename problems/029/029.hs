import Data.List (group, sort)

main :: IO ()
main = putStrLn . show . length . group . sort $ (^) <$> [2..100] <*> [2..100]
