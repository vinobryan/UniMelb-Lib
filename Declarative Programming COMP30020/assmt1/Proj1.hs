module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import Data.Map (fromListWith, toList)

type GameState = [[String]]
type Response = (Int,Int,Int)

-- Get the initial list without any modification
-- Input:
--  None
-- Output:
--  initial list(GameState)
initList :: GameState
initList = [first:second:[third] | first <- combination, second <- combination,
                                   third <- combination,
            first < second && second < third]
            where
              combination = [x:[y] | x <- "ABCDEFG", y <- "123"]


-- Get a map of all note
-- Input:
--  a single guess([String])
-- Output:
--  a string of notes(String)
getNote :: [String] -> String
getNote = map head

-- Get a map of all octaves
-- Input:
--  a single guess([String])
-- Output:
--  a string of octaves(String)
getOctave :: [String] -> String
getOctave = map (head . tail)

-- Get the response of the guess
-- Input:
--  the target([String])
--  the guess([String])
-- Output:
--  the corresponding response(Response)
getResponse :: [String] -> [String] -> Response
getResponse target guess = (right, note, octave)
  where right = length $ intersect guess target
        note = equals (getNote guess) (getNote target) - right
        octave = equals (getOctave guess) (getOctave target) - right

-- Get the number of equaling chars in 2 strings
-- Input:
--  string a(String)
--  string b(String)
-- Output:
--  the number of equaling chars(Int)
equals :: String -> String -> Int
equals [] _ = 0
equals (a:as) b =
  if a `elem` b
    then
      1 + equals as (delete a b)
    else
      0 + equals as b

-- Get a list which has the same response as the input
-- Input:
--  the key response(Response)
--  the target([String])
--  the guess list(GameState)
-- Output:
--  a list without the different response(GameState)
leaveSameResponse :: Response -> [String] -> GameState -> GameState
leaveSameResponse response target guessList =
  [x | x <- guessList, getResponse target x == response]

-- Get a list of responses
-- Input:
--  the target([String])
--  the guess list used to calculate response(GameState)
-- Output:
--  a list of response(Response)
getResponseList :: [String] -> GameState -> [Response]
getResponseList _ [] = []
getResponseList target (g:guessList) =
  getResponse target g : getResponseList target guessList

-- Get a frequency dictionary of response
-- Input:
--  a response list([Response])
-- Output:
--  a dictionary of response([(a, Int)])
responseFrequency :: (Ord a) => [a] -> [(a, Int)]
responseFrequency xs = toList (fromListWith (+) [(x, 1) | x <- xs])

-- Get a list of expected score
-- Input:
--  a single guess([String])
--  a guess list(GameState)
-- Output:
--  a list of expected score
getScoreList :: [String] -> GameState -> [Int]
getScoreList guess guessList =
  map snd (responseFrequency(getResponseList guess guessList))

-- Calculate a single expected score
-- Input:
--  a score list([Int])
--  the total sum in that list(Int)
-- Output:
--  the expected score(Double)
calculateExpectValue :: [Int] -> Int -> Double
calculateExpectValue [] _ = 0
calculateExpectValue (s:scoreList) totalNum =
   fromIntegral (s * s) / fromIntegral totalNum
   + calculateExpectValue scoreList totalNum

-- Get the best expected score from all guesses
-- Input:
--  a guess list(GameState)
--  the same guess list as the previous list(GameState)
--  the initial best expected case([String], Double)
-- Output:
--  the best expected case([String], Double)
getBestExpect :: GameState -> GameState -> ([String], Double)
                 -> ([String], Double)
getBestExpect [] _ best = best
getBestExpect (g:guesses) guessList best =
  let scoreList = getScoreList g guessList
      currentExp = calculateExpectValue scoreList (sum scoreList) in
  if snd best == 0 || currentExp < snd best
    then
      getBestExpect guesses guessList (g, currentExp)
    else
      getBestExpect guesses guessList best

-- The next best guess of the game
-- Input:
--  the state from previous guess([String],GameState)
--  the response from previous guess(Response)
-- Output:
--  the best expected guess([String], GameState)
nextGuess :: ([String],GameState) -> Response -> ([String],GameState)
nextGuess state lastResponse =
  let list = uncurry (leaveSameResponse lastResponse) state
      bestChoice = getBestExpect list list ([], 0) in
  (fst bestChoice, delete (fst bestChoice) list)

-- The initial guess of this game
-- Input:
--  None
-- Output:
--  the first guess of the game([String], GameState)
initialGuess :: ([String], GameState)
initialGuess = (["A1","B1","C2"], initList)
