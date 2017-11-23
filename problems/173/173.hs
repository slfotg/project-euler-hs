
n = 1000000

f x y = x ^ 2 - y ^ 2

g x = takeWhile (<= n) $ map (f x) [x - 2, x - 4 .. 1]

h = takeWhile (\x -> f x (x - 2) <= n) [3 ..]

main = putStrLn . show . sum . map length $ map g h
