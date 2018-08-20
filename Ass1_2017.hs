-- Assignment 1 for COMP30020 Declarative Programming 2017

module Lab1 (subst, interleave, unroll) where

    -- Question 1
    -- Function replaces every occurrence of the first value with the second in list.
    subst :: Eq t => t -> t -> [t] -> [t]
    subst _ _ [] = []
    subst a b (x:xs)
        | x == a = b : subst a b xs
        | otherwise = x : subst a b xs

    -- Question 2
    -- Function interleaves two lists.
    interleave :: [t] -> [t] -> [t]
    interleave xs [] = xs
    interleave [] ys = ys
    interleave (x:xs) (y:ys) = x : y : interleave xs ys

    -- Question 3
    -- Function unrolls the list by input value as size.
    unroll :: Int -> [a] -> [a]
    unroll n xs
        | n <= size = take n xs
        | otherwise = xs ++ unroll (n - size) xs
        where size = length xs
