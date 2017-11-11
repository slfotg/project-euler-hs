
readNumbers :: String -> IO [Integer]
readNumbers = fmap (map read) . fmap lines . readFile

main :: IO ()
main = do
    numbers <- readNumbers "013.txt"
    putStrLn . (take 10) . show $ sum numbers
