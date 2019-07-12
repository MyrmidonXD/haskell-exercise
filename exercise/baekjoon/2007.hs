{-
    [Baekjoon Online Judge] 1924. Year 2007
    https://www.acmicpc.net/problem/1924
-}

import Data.Char

data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat deriving (Eq, Ord, Enum, Show)

monthDateToDateCount :: Int -> Int -> Int
monthDateToDateCount m d = sum $ d:take (m - 1) dateCountForMonths
  where dateCountForMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

monthDateToDayIn2007 :: Int -> Int -> Day
monthDateToDayIn2007 m d =  dayList !! rem dateCount 7
  where dateCount = monthDateToDateCount m d
        dayList = [Sun, Mon, Tue, Wed, Thu, Fri, Sat]

solution m d = map toUpper . show $ monthDateToDayIn2007 m d

main = do
    line <- getLine
    let m = read $ takeWhile (/= ' ') line :: Int
    let d = read $ tail $ dropWhile (/= ' ') line :: Int
    putStrLn $ solution m d
