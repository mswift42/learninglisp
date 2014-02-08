-- CSE 341, Spring 2013, Assignment 2 Due: Friday April 19, 11:00PM You
-- will write 11 SML functions (not counting local helper functions), 4
-- related to \name substitutions" and 7 related to a made-up solitaire
-- card game.  Your solutions must use pattern-matching. You may not use
-- the functions null , hd , tl , isSome , or valOf , nor may you use
-- anything containing a # character or features not used in class (such
-- as mutation). Note that list order does not matter unless
-- speci
-- stated in the problem.  Download hw2provided.sml from the course
-- website.  The provided code
-- de
-- several types for you. You will not need to add any additional
-- datatype bindings or type synonyms.

allExceptOption :: String -> [String] -> [String]
allExceptOption s [] = []
allExceptOption s (x:xs) | x == s = xs
                         | otherwise = x : allExceptOption s xs


--getSubstitutions :: [String] -> String ->  [String]
getSubstitutions [] _ = []
getSubstitutions (x:xs) s = go (x:xs) s [] where
  go [] _ _ = []
  go (x:xs) s acc = go xs s acc ++ allExceptOption s x

data Suit = Clubs | Diamonds | Spades | Hearts deriving Show
data Rank = Jack | Queen | King | Ace | Num Int deriving (Eq,Show)
type Card = (Suit, Rank)
data Color = Black | Red

cardColor :: Card -> Color
cardColor (Clubs,_) = Black
cardColor (Spades,_) = Black
cardColor (_,_) = Red

cardValue :: Card -> Int
cardValue (_,rank) = case rank of
  Num n -> n
  Ace -> 11
  _ -> 10

removeCard :: [Card] -> Card -> [Card]
removeCard [] _ = fail "Card not in List"
removeCard (x:xs) c | x == c = xs
                    | otherwise = x : removeCard xs c

allSameColor :: [Card] -> Bool
allSameColor xs = all (==s) $ map cardColor xs
  where s = cardColor (head xs)
