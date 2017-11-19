import Data.Char (ord)
import qualified Data.Set as Set

triangles :: [Int]
triangles = scanl (+) 1 [2..]

triangleSet :: Set.Set Int
triangleSet = Set.fromList $ take 1000 triangles

isTriangle :: Int -> Bool
isTriangle = (`Set.member` triangleSet)

isTriangleWord :: String -> Bool
isTriangleWord = isTriangle . wordValue

readWords :: String -> [String]
readWords line = read ("[" ++ line ++ "]")

readWordsFromFile :: String -> IO [String]
readWordsFromFile file = fmap readWords $ readFile file

charToInt :: Char -> Int
charToInt c = (ord c) - (ord 'A') + 1

wordValue :: String -> Int
wordValue = sum . map charToInt

triangleWords :: [String] -> [String]
triangleWords = filter isTriangleWord

main :: IO ()
main = do
    wrds <- readWordsFromFile "problems/042/042.txt"
    putStrLn . show . length $ triangleWords wrds
