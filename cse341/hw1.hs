data Date = D Int Int Int


year :: Date -> Int
year (D _ _ y) = y

month :: Date -> Int
month (D _ m _) = m

day :: Date -> Int
day (D d _ _ ) = d

sumdays :: Date -> Int
sumdays d = (year d) * 365 + (month d) * 12 + (day d)

is_older:: Date -> Date -> Bool
is_older d1 d2 = sumdays d1 > sumdays d2
