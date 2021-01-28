parseTimeString :: String -> [String]
parseTimeString ts = 
    let period = drop 8 ts
        time   = take 8 ts
    in period : splitStr ':' time

splitStr :: Char -> String -> [String]
splitStr _ ""    = []
splitStr delim s = 
    let front = takeWhile (/= delim) s
        n     = length front
    in front : (splitStr delim $ drop (n + 1) s)

joinTimeString :: [String] -> String
joinTimeString ([digit]:rest) = joinStr ':' $ ('0':[digit]):rest
joinTimeString tss            = joinStr ':' tss

joinStr :: Char -> [String] -> String
joinStr _ []         = ""
joinStr delim [s]    = s
joinStr delim (s:ss) = s ++ (delim : joinStr delim ss)

timeConversion :: [String] -> [String]
timeConversion (period:time) = 
    let hour = (read $ head time) `mod` 12
    in case period of
        "AM" -> show hour : tail time
        "PM" -> show (hour + 12) : tail time

main :: IO ()
main = interact $ joinTimeString . timeConversion . parseTimeString
