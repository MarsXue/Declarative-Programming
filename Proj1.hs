--  File     : Proj1.hs
--  Author   : Wenqing Xue (wenqingx)
--  Origin   : Sat Aug 18 2018
--  Purpose  : Guess the correct lineup as less as possible

module Proj1 (Person, parsePerson, height, hair, sex,
              GameState, initialGuess, nextGuess, feedback) where

import Data.List
import Data.Ord

-- Person contains: height (Height), hair colour (Hair), gender (Sex)
data Person = Person Height Hair Sex
    deriving (Show, Eq, Ord)

-- Height contains: short (S), tall (T)
data Height = S | T
    deriving (Show, Eq, Ord, Enum, Read)

-- Hair colour contains: blonde (B), redhead (R), dark haired (D)
data Hair = B | R | D
    deriving (Show, Eq, Ord, Enum, Read)

-- Gender contains: male (M), female (F)
data Sex = M | F
    deriving (Show, Eq, Ord, Enum, Read)

-- Game state information contains: list of all possible lineups
data GameState = GameState {guesses :: [[Person]]} deriving (Show)

-- List all the possible lineups
lineups :: [[Person]]
lineups = [[a, b] | a <- persons, b <- persons, a < b]
    where persons = [Person h c g | h <- [S, T], c <- [B, R, D], g <- [M, F]]

-- Return the count if two inputs are matched
match :: Eq t => [t] -> [t] -> Int
match [] _ = 0
match _ [] = 0
match (x:xs) ys
    | x `elem` ys = 1 + match xs (delete x ys)
    | otherwise = match xs ys

-- Take a three-character string and returns Just person
parsePerson :: String -> Maybe Person
parsePerson (h:c:g:[])
    | checkValid = Just (Person (read [h]) (read [c]) (read [g]))
    | otherwise = Nothing
    where
        checkValid = (h `elem` "ST") && (c `elem` "BRD") && (g `elem` "MF")

-- Return the person's height
height :: Person -> Height
height (Person h _ _) = h

-- Return the person's hair colour
hair :: Person -> Hair
hair (Person _ c _) = c

-- Return the person's sex
sex :: Person -> Sex
sex (Person _ _ g) = g

-- count the frequency
frequency :: Ord a => [a] -> [Int]
frequency lst = map (\x -> length x) . group . sort $ lst

-- calculate the expected number
calculate :: [Int] -> Double
calculate lst = (fromIntegral (sum [x * x | x <- lst])) / (fromIntegral (sum lst))

calculateGuess guesses = findMin [calculate (frequency [feedback x y | y <- guesses]) | x <- guesses]

-- find the index of minimum value
findMin :: Ord a => [a] -> Int
findMin [] = 0
findMin xs
    | head xs == minimum xs = 0
    | otherwise = 1 + findMin (tail xs)


-- Return the correct suspect, heights, hair colours and sexes in order
feedback :: [Person] -> [Person] -> (Int, Int, Int, Int)
feedback xs ys = (nPerson, nHeight, nHair, nSex)
    where
        nPerson = match xs ys
        nHeight = match [height x | x <- xs] [height y | y <- ys] - nPerson
        nHair   = match [hair x | x <- xs] [hair y | y <- ys] - nPerson
        nSex    = match [sex x | x <- xs] [sex y | y <- ys] - nPerson

-- Return the initial lineup guess and initial game state
initialGuess :: ([Person], GameState)
initialGuess = (guess, GameState {guesses = allGuesses})
    where
        guess = lineups !! (expNum lineups)
        allGuesses = delete guess lineups

-- Return the next lineup guess and new game state
nextGuess :: ([Person], GameState) -> (Int, Int, Int, Int) -> ([Person], GameState)
nextGuess (oldGuess, gs) score = (newGuess, GameState {guesses = allGuesses})
    where
        posGuesses = [x | x <- guesses gs, feedback oldGuess x == score]
        newGuess = posGuesses !! (expNum posGuesses)
        allGuesses = delete newGuess posGuesses
