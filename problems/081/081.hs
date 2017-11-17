
readArray :: String -> [Int]
readArray line = read ("[" ++ line ++ "]")

readNumbers :: String -> IO [[Int]]
readNumbers file = fmap (map readArray) . fmap lines $ readFile file

getMinRow :: Integral a => [a] -> [a] -> [a]
getMinRow (m:ms) (x:xs) = let n = (m + x) in
    n : getMinRow' n ms xs
        where
            getMinRow' :: Integral a => a -> [a] -> [a] -> [a]
            getMinRow' p (m:ms) (x:xs) = let n = (min p m) + x in
                n : getMinRow' n ms xs
            getMinRow' _ _ [] = []
getMinRow [] (x:xs) = scanl (+) x xs

-- matrix = [ [131, 673, 234, 103,  18]
--          , [201,  96, 342, 965, 150]
--          , [630, 803, 746, 422, 111]
--          , [537, 699, 497, 121, 956]
--          , [805, 732, 524,  37, 331] ] :: [[Int]]

main :: IO ()
main =   putStrLn . show
     =<< fmap (last . foldl getMinRow []) (readNumbers "problems/081/081.txt")
