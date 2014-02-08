-- CSE 341, Spring 2013, Assignment 1

data Date = D Int Int Int


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
