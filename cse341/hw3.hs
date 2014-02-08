-- CSE 341, Spring 2013, Assignment 3 Due: Monday April 29, 11:00PM You
-- will
-- de
-- several SML functions. Many will be very short because they will use
-- other higher-order functions. You may use functions in ML's library;
-- the problems point you toward the useful functions and often require
-- that you use them. The sample solution is about 120 lines, including
-- the provided code, but not including the challenge problem. This
-- assignment is probably more dicult than Homework 2 even though
-- (perhaps because) many of the problems have 1-line answers.

import Data.Char

onlyCapitals :: [String] -> [String]
onlyCapitals = filter (isUpper . head)

longestString1 :: [String] -> String
longestString1 [] = ""
longestString1 xs = foldl1 (\x y -> if length x > length y then x else y ) xs
