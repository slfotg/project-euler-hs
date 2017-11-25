import Data.List

rotate90 :: [[a]] -> [[a]]
rotate90 = reverse . transpose

rotate180 :: [[a]] -> [[a]]
rotate180 = rotate90 . rotate90

diagonals :: [[a]] -> [[a]]
diagonals = (++) <$> transpose . zipWith drop [0..]
                 <*> transpose . zipWith drop [1..] . rotate180

allDiagonals :: [[a]] -> [[a]]
allDiagonals m = diagonals m ++ (diagonals $ rotate90 m)

allDirections :: [[a]] -> [[a]]
allDirections m = m ++ (rotate90 m) ++ allDiagonals m

products :: Num a => [a] -> [[a]]
products arr = products' (tail arr) arr
    where
        products' [] arr' = [arr']
        products' fs arr' = arr' : (products' (tail fs) $ dotProduct fs arr')

dotProduct :: Num a => [a] -> [a] -> [a]
dotProduct a = zipWith (*) a

maxProduct :: Int -> [Int] -> Maybe Int
maxProduct n xs = fmap maximum $ (products xs) !!! (n - 1)

(!!!) :: [a] -> Int -> Maybe a
(!!!) (a:as) 0 = return a
(!!!) []     _ = Nothing
(!!!) (a:as) n = as !!! (n - 1)

max' :: Ord a => Maybe a -> a -> a
max'  Nothing b = b
max' (Just a) b = max a b

maximum' :: (Num a, Ord a) => [Maybe a] -> a
maximum' = foldr max' 0

readMatrix :: String -> IO [[Int]]
readMatrix file = do
    ls <- fmap lines $ readFile file
    return . map (map read) $ map words ls

main :: IO ()
main = do
    matrix <- readMatrix "problems/011/011.txt"
    putStrLn . show . maximum' . map (maxProduct 4) $ allDirections matrix
