
readNumbers :: String -> IO [[Int]]
readNumbers file = do
    numbers <- fmap (map words) . fmap lines $ readFile file
    return $ map (map read) numbers

getPartialMax :: [Int] -> [Int] -> [Int]
getPartialMax top@(x:_) (y:ys) = (x + y) : getPartialMax' top ys
    where
        getPartialMax' (a:b:cs) (n:ns) = max (n + a) (n + b) : getPartialMax' (b:cs) ns
        getPartialMax' (a:[]) (n:_)    = [a + n]
getPartialMax [] ys = ys

getMax :: [[Int]] -> Int
getMax = maximum . foldl getPartialMax []

main :: IO ()
main = fmap getMax (readNumbers "problems/067/067.txt") >>= putStrLn . show 
