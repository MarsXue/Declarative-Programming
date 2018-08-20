-- Assignment 1 for COMP30020 Declarative Programming 2018
module Assignment1 (elementPosition, everyNth, sumLater, sumEarlier) where

    -- Question 1
    -- Function returns the first occurrence of element in list.
    elementPosition :: Eq t => t -> [t] -> Int
    elementPosition elt lst
        | head lst == elt = 0
        | null (tail lst) = 1
        | otherwise = 1 + elementPosition elt (tail lst)

    -- Question 2
    -- Function repeats every n th elements of list.
    everyNth :: Int -> [t] -> [t]
    everyNth n xs
        | n <= 0 = error "error"
        | n <= size = last (take n xs) : everyNth n (drop n xs)
        | otherwise = []
        where size = length xs

    -- Question 3
    -- Function sums the input list backwards.
    sumLater :: Num a => [a] -> [a]
    sumLater [] = []
    sumLater xs = sum xs : sumLater (tail xs)

    -- Question 4
    -- Function sums the input list forwards.
    sumEarlier :: Num a => [a] -> [a]
    sumEarlier [] = []
    sumEarlier xs = sumEarlier (init xs) ++ [sum xs]
