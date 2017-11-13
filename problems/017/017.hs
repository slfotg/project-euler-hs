
hundred :: Int
hundred = length "hundred"

andLength :: Int
andLength = length "and"

numbers :: Int -> Int
numbers  0 = 0
numbers  1 = length "one"
numbers  2 = length "two"
numbers  3 = length "three"
numbers  4 = length "four"
numbers  5 = length "five"
numbers  6 = length "six"
numbers  7 = length "seven"
numbers  8 = length "eight"
numbers  9 = length "nine"
numbers 10 = length "ten"
numbers 11 = length "eleven"
numbers 12 = length "twelve"
numbers 13 = length "thirteen"
numbers 14 = length "fourteen"
numbers 15 = length "fifteen"
numbers 16 = length "sixteen"
numbers 17 = length "seventeen"
numbers 18 = length "eighteen"
numbers 19 = length "nineteen"
numbers 20 = length "twenty"
numbers 30 = length "thirty"
numbers 40 = length "forty"
numbers 50 = length "fifty"
numbers 60 = length "sixty"
numbers 70 = length "seventy"
numbers 80 = length "eighty"
numbers 90 = length "ninety"
numbers 1000 = (numbers 1) + length "thousand"
numbers x = if x >= 100
    then
        let (d, m) = (x `div` 100, x `mod` 100) in
            if m == 0
                then hundred + (numbers d)
                else hundred + (numbers d) + andLength + (numbers m)
    else
        let (d, m) = (x `div` 10, x `mod` 10) in
            (numbers (d * 10)) + (numbers m)

main :: IO ()
main = putStrLn . show . sum $ map numbers [1..1000]
