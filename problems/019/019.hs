
type Date = (Int, Int, Int)

daysInMonth :: Date -> Int
daysInMonth (year, 1, _)
    | year `mod` 4 == 0   = 29
    | otherwise           = 28
daysInMonth (_, month, _) = case month `elem` [3, 5, 8, 10] of
    True  -> 30
    False -> 31

incrementDayOfWeek :: Int -> Int -> Int
incrementDayOfWeek dow n = (dow + n) `mod` 7

incrementMonth :: Date -> Date
incrementMonth      (year,    11, dayOfWeek) =
    ((year + 1), 0, incrementDayOfWeek dayOfWeek 31)
incrementMonth date@(year, month, dayOfWeek) =
    (year, (month + 1), incrementDayOfWeek dayOfWeek (daysInMonth date))

-- 1 Jan 1900 was a Monday.
startDate :: Date
startDate = (1901, 0, incrementDayOfWeek 1 365)

isSunday :: Date -> Bool
isSunday (_, _, 0) = True
isSunday _         = False

main :: IO ()
main = putStrLn . show
     . length
     . filter isSunday
     . takeWhile (< (2001, 0, 0))
     $ iterate incrementMonth startDate
