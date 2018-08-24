--  File     : Proj1.hs
--  Author   : Wenqing Xue
--  Origin   : Sat Aug 18 2018
--  Purpose  : Program for project 1

module Proj1 (Person, parsePerson, height, hair, sex,
              GameState, initialGuess, nextGuess, feedback) where

import Data.List

data Person = Person Height Hair Sex
    deriving (Show, Eq, Ord)

data Height = S | T
    deriving (Show, Eq, Ord, Enum, Read)

data Hair = B | R | D
    deriving (Show, Eq, Ord, Enum, Read)

data Sex = M | F
    deriving (Show, Eq, Ord, Enum, Read)

-- game state information
data GameState = GameState {guesses :: [[Person]]} deriving (Show)


allLineups :: [[Person]]
allLineups = [[a, b] | a <- allPersons, b <- allPersons, a < b]
    where allPersons = [Person h c g | h <- [S, T],
                                       c <- [B, R, D],
                                       g <- [M, F]]


checkMatch :: Eq t => [t] -> [t] -> Int
checkMatch [] _ = 0
checkMatch (x:xs) ys
    | elem x ys = 1 + checkMatch xs (delete x ys)
    | otherwise = checkMatch xs ys


checkEqual :: [Person] -> [Person] -> Int
checkEqual (a:b:[]) (x:y:[]) = if p > q then p else q
  where
      p = equalPerson a x + equalPerson b y
      q = equalPerson a y + equalPerson b x


equalPerson :: Person -> Person -> Int
equalPerson (Person a b c) (Person x y z)
    | (a == x) && (b == y) && (c == z) = 1
    | otherwise = 0


parsePerson :: String -> Maybe Person
parsePerson (h:c:g:[])
    | checkValid = Just (Person (read [h]) (read [c]) (read [g]))
    | otherwise = Nothing
        where
            checkValid = (h == 'S' || h == 'T') &&
                         (c == 'B' || c == 'R' || c == 'D') &&
                         (g == 'M' || g == 'F')


height :: Person -> Height
height (Person h _ _) = h


hair :: Person -> Hair
hair (Person _ c _) = c


sex :: Person -> Sex
sex (Person _ _ g) = g


feedback :: [Person] -> [Person] -> (Int, Int, Int, Int)
feedback (a:b:[]) (x:y:[]) = (np, nh, nc, ng)
    where
        np = checkEqual (a:b:[]) (x:y:[])
        nh = checkMatch (height a : [height b]) (height x : [height y]) - np
        nc = checkMatch (hair a : [hair b]) (hair x : [hair y]) - np
        ng = checkMatch (sex a : [sex b]) (sex x : [sex y]) - np


initialGuess :: ([Person], GameState)
initialGuess =
    let guess = head allLineups
    in (guess, GameState {guesses = tail allLineups})


nextGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> ([Person], GameState)
nextGuess (prevGuess, gs) score = (head newGuesses, newGs)
    where
        newGuesses = [x | x <- guesses gs, feedback prevGuess x == score]
        newGs = GameState {guesses = tail newGuesses}
