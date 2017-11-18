import Data.List
import Data.Char

readNames :: String -> [String]
readNames line = read ("[" ++ line ++ "]")

readNamesFromFile :: String -> IO [String]
readNamesFromFile file = fmap sort . fmap readNames $ readFile file

charToInt :: Char -> Int
charToInt c = (ord c) - (ord 'A') + 1

nameValue :: String -> Int
nameValue = sum . map charToInt

main :: IO ()
main = do
    names <- readNamesFromFile "problems/022/022.txt"
    putStrLn . show . sum . zipWith (*) [1..] $ map nameValue names
