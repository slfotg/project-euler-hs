
reverseInt :: Integer -> Integer
reverseInt = read . reverse . show

isPalindrome :: Integer -> Bool
isPalindrome n = reverseInt n == n

isLychrel :: Integer -> Bool
isLychrel n = isLychrel' 50 n
    where
        isLychrel' :: Int -> Integer -> Bool
        isLychrel' 0 _ = True
        isLychrel' i m = let p = m + reverseInt m in
            if isPalindrome p
                then False
                else isLychrel' (i - 1) p

main :: IO ()
main = putStrLn . show . length $ filter isLychrel [1 .. 9999]
