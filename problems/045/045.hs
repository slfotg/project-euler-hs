
triangles   = map (\x -> x * (x + 1) `div` 2)     [1 ..]
pentagonals = map (\x -> x * (3 * x - 1) `div` 2) [1 ..]
hexagonals  = map (\x -> x * (2 * x - 1))         [1 ..]

getCommon (t:ts) (p:ps) (h:hs)
    | t == p && t == h = t : getCommon ts ps hs
    | otherwise = let m = minimum [t, p, h] in
        if t == m
            then getCommon ts (p:ps) (h:hs)
            else if p == m
                then getCommon (t:ts) ps (h:hs)
                else getCommon (t:ts) (p:ps) hs

main :: IO ()
main = putStrLn . show $ getCommon triangles pentagonals hexagonals !! 2
