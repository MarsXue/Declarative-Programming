import Proj1

fromJust (Just a) = a

c1 = fromJust (parsePerson "SRF")
c2 = fromJust (parsePerson "TBM")

loop :: [Person] -> [Person] -> Proj1.GameState -> Int -> Int
loop target guess other guesses = do

    let answer = feedback target guess

    if answer == (length target,0,0,0)
      then do
          guesses
      else do
          let (guess',other') = nextGuess (guess,other) answer
          loop target guess' other' (guesses+1)


pPerson = [fromJust (parsePerson (he++ha++s))
                      | he <- ["S", "T"],
                        ha <- ["B", "R", "D"],
                        s  <- ["M", "F"]]
pCases = [[pa, pb] | pa <- pPerson,
                    pb <- pPerson,
                    pa < pb]

                    -- (gu, gs) = initialGuess
                    -- loop ([c1, c2]) gu gs 1
runTest :: [[Person]] -> Int
runTest [] = 0
runTest (c:cs) = (loop c gu gs 1) + (runTest cs)
    where
        (gu, gs) = initialGuess

score = (fromIntegral (runTest pCases)) / (fromIntegral (length pCases))
