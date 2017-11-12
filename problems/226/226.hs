-- integral of blancmange curve from a to b
integral a b = integral' b 0 - integral' a 0
    where 
        integral' x 40
            | x > 0.5   = 0.5
            | otherwise = x * x * 0.5
        integral' x n
            | x > 0.5   = 0.5 - integral' (1.0 - x) (n+1)
            | otherwise = ((integral' (2 * x) (n+1)) / 4.0) + (x * x * 0.5)

-- function of bottom half of circle
circ x = 0.5 - sqrt (0.5 * x - x * x)

-- integral of bottom half of circle from a to b
circInt a b = circAnt b - circAnt a
    where
        circAnt x = (1 / sqrt ( 0.5*x - x*x)) * (x * (0.5 * sqrt ( 0.5*x - x*x) + 0.5*x*x - 0.375*x + 0.0625) - 0.0625 * sqrt (0.5*x - x*x) * asin (sqrt (2 * x)))

-- get only decimal part of number
fPart f = f - (fromIntegral (floor f))

-- sawtooth function starting at 0 with period 1
sawtooth x
    | 0 <= (fPart x) && (fPart x) < 0.5  = (fPart x)
    | (fPart x) >= 0.5 && (fPart x) <= 1 = 1 - (fPart x)

-- blancmange curve function 
blancmange x 40 = sawtooth ((fromIntegral (2 ^ 40)) * x) / (fromIntegral (2 ^ 40))
blancmange x i  = sawtooth ((fromIntegral (2 ^ i)) * x) / (fromIntegral (2 ^ i)) + blancmange x (i+1)

-- search for circle and blancmange intersection
search p1 p2 i
    | i == 50   = ((p1 + p2) * 0.5)
    | otherwise =
        if (blancmange mid 0) > (circ mid)
            then search p1 mid (i+1)
            else search mid p2 (i+1)
                where
                    mid = (p1 + p2) * 0.5

-- rounds f to p decimal places
roundF f p = (fromIntegral (round (f * 10^p))) / (fromIntegral (10 ^ p))

main = putStrLn (take 10 (show (roundF (((integral a 0.5) - (circInt a 0.49999999999))) 8)))
    where
        a = search 0 0.25 0
