import qualified Data.Set as Set

integerValue :: [Integer]-> Integer
integerValue = foldl ((+) . (*10)) 0

getPermutations
    :: Set.Set Integer -- ^ Set of values that can be used
    -> [Integer]       -- ^ Values that have been used already
    -> Integer         -- ^ second to last integer used
    -> Integer         -- ^ last integer used
    -> [Integer]       -- ^ value the last 3 digits need to be a factor of
    -> [Integer]       -- ^ set of all permutations
getPermutations set used a b (p:ps) =
    if (length used > 0 && integerValue used == 0)
        then []
        else foldr (++) [] . map getPermutations' $ Set.elems set
        where
            getPermutations' c
                | (integerValue [a, b, c] `mod` p) == 0 =
                    getPermutations (Set.delete c set) (used ++ [c]) b c ps
                | otherwise = []
getPermutations _   used _ _ [] = [integerValue used]

main :: IO ()
main = putStrLn . show . sum
     $ getPermutations (Set.fromList [0..9]) [] 0 0 [1,1,1,2,3,5,7,11,13,17]
