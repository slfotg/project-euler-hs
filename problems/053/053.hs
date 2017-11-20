
sierpinski :: [[Integer]]
sierpinski = iterate sierpinski' [1]
    where
        sierpinski' xs = 1 : next xs
        next (a:b:cs) = (a + b) : next (b:cs)
        next (_:[])   = [1]

main :: IO ()
main = putStrLn . show
     . sum
     . map length
     . map (filter (> 1000000))
     $ take 101 sierpinski
