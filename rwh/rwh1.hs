-- Exercises for Real World Haskell

-- Use a fold (choosing the appropriate fold will make your code much
-- simpler) to rewrite and improve upon the asInt function from the
-- section called “Explicit recursion”
import Data.Char

asIntFold :: String -> Int
asIntFold ('-':xs) = -(asIntFold xs)
asIntFold xs = foldl (\x y -> x * 10 + digitToInt y) 0 xs

concat' :: [[a]] -> [a]
concat' = foldr (++) []
