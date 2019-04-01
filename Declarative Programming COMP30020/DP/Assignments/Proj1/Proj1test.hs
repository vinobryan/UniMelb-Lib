--  File     : Proj1test.hs
--  Author   : Peter Schachte
--  Purpose  : Test program for proj1 project

module Main (main, guessTest) where

import Data.List
import Card
import Proj1
import System.Exit
import System.Environment

type Selection = [Card]
type Feedback = (Int, Int, Int, Int, Int)


-- |For a given number of cards in the game, gives the target number
--  of guesses per game, ie, the number of guesses to average to get
--  full marks for guessing quality.
goldStandard :: Int -> Double
goldStandard cards
  | cards <= 2 = 4
  | otherwise  = 5


-- |The number of guesses per game at (or beyond) which one earns 0.0 marks.
zeroPoint :: Double
zeroPoint = 663


-- | Main program for use inside ghci.  Do something like
--
--     guessTest "4C 9D"
--
--   to use initialGuess and nextGuess to guess the answer 4C, 9D.
guessTest :: String -> IO ()
guessTest answerString = do
    let answer = map read $ words answerString
    if validSelection answer then do
        let (guess,other) = initialGuess $ length answer
        loop answer guess other 1
    else do
      putStrLn "Invalid answer:  input must be a string of one or more"
      putStrLn "distinct cards separated by whitespace, where each card"
      putStrLn "is a single character rank 2-9, T, J, Q, K or A, followed"
      putStrLn "by a single character suit C, D, H, or S."
    


-- | Main program.  Gets the answer from the command line (as separate
--   command line arguments, each a rank character (2-9, T, J, Q, K or
--   A) followed by a suit letter (C, D, H, or S).  Runs the user's
--   initialGuess and nextGuess functions repeatedly until they guess
--   correctly.  Counts guesses, and prints a bit of running
--   commentary as it goes.
main :: IO ()
main = do
  args <- getArgs
  let answer = map read args
  if validSelection answer then do
      let (guess,other) = initialGuess $ length answer
      loop answer guess other 1
    else do
      putStrLn "Usage:  Proj1test c1 ... cn"
      putStrLn "   where c1 ... cn are different cards between 2C and AS"
      exitFailure


-- | The guessing loop.  Repeatedly call nextGuess until the correct answer 
--   is guessed.
loop :: Selection -> Selection -> GameState -> Int -> IO ()
loop answer guess other guesses = do
    putStrLn $ "Your guess " ++ show guesses ++ ":  " ++ show guess
    if validSelection guess && length answer == length guess then do
        let result = feedback answer guess
        putStrLn $ "My answer:  " ++ show result
        if successful guess result then do
            putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
            putStrLn $ "Approximate quality = " 
                        ++ show (100 * qualityFraction
                                       (length answer) 1 guesses)
                        ++ "%"
          else do
            let (guess',other') = nextGuess (guess,other) result
            loop answer guess' other' (guesses+1)
      else do
        putStrLn "Invalid guess"
        exitFailure

-- | Compute the proportion of possible marks that should be awarded for
--   quality given the number of guesses taken and the number of games
--   played. We take an average of 4 guesses per game to be the gold
--   standard for 2 card games, and 5 guesses for bigger games. Guessing
--   the target in this few guesses (or less) earns full marks (1.0).  There
--   are 1326 combinations of 2 cards, so guessing every pair, we would
--   expect to guess the right pair with 663 guesses, so that number of
--   guesses or more earns 0.0 for quality. Between there we use a
--   logarithmic scale.  For the harder cases, with 3 or more cards, we
--   expect more, so we stick to 663 guesses as the point for 0 quality.
qualityFraction :: Int -> Int -> Int -> Double
qualityFraction cards plays guesses =
    min 1
    $ max 0
    $ 1 - logBase (zeroPoint / perfect) (guessesPerPlay / perfect)
    where perfect = goldStandard cards
          guessesPerPlay = fromIntegral guesses / fromIntegral plays


-- | Returns whether or not the feedback indicates the guess was correct.
successful :: Selection -> Feedback -> Bool
successful sel (right,_,_,_,_) = right == length sel


-- | Returns whether or not a guess or answer is valid, ie, has no repeats.
validSelection :: [Card] -> Bool
validSelection sel = sel == nub sel && not (null sel)
