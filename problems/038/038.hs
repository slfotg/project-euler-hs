import Data.List (sort)

pandigital :: Int -> Maybe Integer
pandigital n = pandigital' "" [1..]
    where
        pandigital' str (f:fs)
            | length str  > 9 = Nothing
            | length str == 9 =
                if (sort str) == "123456789"
                    then return $ read str
                    else Nothing
            | otherwise       = pandigital' (str ++ (show (f * n))) fs

maybeMax :: Maybe Integer -> Integer -> Integer
maybeMax Nothing  b = b
maybeMax (Just a) b = max a b

main :: IO ()
main = putStrLn . show . foldr maybeMax 0 $ map pandigital [1 .. 9876]
