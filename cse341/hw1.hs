-- CSE 341, Spring 2013, Assignment 1

data Date = D Int Int Int deriving Show

year :: Date -> Int
year (D _ _ y) = y

month :: Date -> Int
month (D _ m _) = m

day :: Date -> Int
day (D d _ _ ) = d

sumDays :: Date -> Int
sumDays d = year d * 365 + month d * 12 + day d

isOlder:: Date -> Date -> Bool
isOlder d1 d2 = sumDays d1 > sumDays d2

numberInMonth :: [Date] -> Int -> Int
numberInMonth xs m = length $ filter (== m) $ map month xs

numberInMonths :: [Date] -> [Int] -> Int
numberInMonths _ [] = 0
numberInMonths dates (x:xs) = numberInMonth dates x +  numberInMonths dates xs

datesInMonth :: [Date] -> Int -> [Date]
datesInMonth dates m = filter (\x -> month x == m) dates

datesInMonths :: [Date] -> [Int] -> [Date]
datesInMonths _ [] = []
datesInMonths dates (x:xs) = datesInMonth dates x ++ datesInMonths dates xs

getNth :: [a] -> Int -> a
getNth (x:xs) 1 = x
getNth (x:xs) n = getNth xs (n - 1)

dateToString :: Date -> String
dateToString d = getNth months (month d) ++ " " ++ show (day d) ++ ", " ++ show (year d)
                 where months = [ "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
