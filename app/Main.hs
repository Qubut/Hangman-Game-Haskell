module Main
    ( main
    , runGame
    ) where

import Control.Monad (forever)
import Data.Char (toLower)
import System.Exit (exitSuccess)
import System.Console.ANSI
import Hangman
    ( HangmanState(..)
    , printHangman
    , printSavedHangman
    , initialHangmanState
    , updateHangmanState
    )
import Puzzle(
    Puzzle(..)
    , Word'(..)
    , TriedCharacters(..)
    , freshPuzzle
    , handleGuess
    , alreadyGuessed
    , displayPuzzle'sWord
    , isAllCorrect
    )
import WordsList (randomWord, extraTries)

-- | Checks if the game is over and handles the logic accordingly.
gameOver :: Puzzle -> IO ()
gameOver (Puzzle (Word' word) guessed (TriedCharacters tries)) =
  if length tries > chances && not (isAllCorrect guessed) then do
    putStrLn "You lose!"
    displayPuzzle'sWord (Word' word)
    printHangman (HangmanState chances)
    exitSuccess
  else return ()
  where
    chances = length word + extraTries

-- | Checks if the game is won and handles the logic accordingly.
gameWin :: Puzzle -> IO ()
gameWin (Puzzle (Word' word) guessed _) =
  if isAllCorrect guessed then do
    putStrLn "You win!"
    displayPuzzle'sWord (Word' word)
    printSavedHangman
    exitSuccess
  else return ()

-- | Runs the game loop, handling each guess and updating the hangman state.
runGame :: Puzzle -> HangmanState -> IO ()
runGame puzzle hangmanState = forever $ do
  clearScreen
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  printHangman hangmanState
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> do
      puzzle' <- handleGuess puzzle c
      let hangmanState' = if alreadyGuessed puzzle' c then hangmanState else updateHangmanState hangmanState
      runGame puzzle' hangmanState'
    _ -> putStrLn "Your guess must be a single character"

-- | Entry point of the program.
main :: IO ()
main = do
  word <- randomWord
  let puzzle = freshPuzzle (fmap toLower word)
      hangmanState = initialHangmanState
  runGame puzzle hangmanState