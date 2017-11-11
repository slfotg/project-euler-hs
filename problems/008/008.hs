import Data.Char
import Data.List
import System.IO

readDigits :: String -> IO [Int]
readDigits file = do
    lines <- fmap lines $ readFile file
    return . map digitToInt $ intercalate [] lines

getMaxProduct :: Int -> [Int] -> Int
getMaxProduct iterations digits = getMaxProduct' 1 digits digits
    where
        getMaxProduct' i digits'@(_:ds) products
            | i == iterations = maximum products
            | otherwise       = getMaxProduct' (i + 1) ds $ zipWith (*) digits' products

main :: IO ()
main = do
    digits <- readDigits "008.txt"
    putStrLn . show $ getMaxProduct 13 digits
