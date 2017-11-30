import qualified Data.Set as Set

sums :: (a -> Bool) -> (a -> a -> a) -> [a] -> [[a]]
sums w f arr = sums' (tail arr) arr
    where
        sums' _  []   = []
        sums' [] arr' = [takeWhile w arr']
        sums' fs arr' = (takeWhile w arr') : (sums' (tail fs)
                                           $ (zipWhileWith w f) fs arr')

zipWhileWith :: (c -> Bool) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWhileWith w f (a:as) (b:bs)
    | w c = c : zipWhileWith w f as bs
    | otherwise = []
        where
            c = f a b
zipWhileWith _ _ _ [] = []
zipWhileWith _ _ [] _ = []

isPalindrome :: Show a => a -> Bool
isPalindrome a = let str = show a in reverse str == str

allSums :: Integer -> [[Integer]]
allSums n = tail . sums (< n) (+) $ map (^2) [1..]

palindromicSums :: Integer -> Set.Set Integer
palindromicSums = Set.unions
                . map (Set.fromList . filter isPalindrome) . allSums

main :: IO ()
main = print . sum $ palindromicSums (10 ^ 8)
