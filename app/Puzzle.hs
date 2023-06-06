module Puzzle
    (
        Puzzle(..)
        , Word'(..)
        , GuessedCharacters(..)
        , TriedCharacters(..)
        , freshPuzzle
        , handleGuess
        , alreadyGuessed
        , displayPuzzle'sWord
        , isAllCorrect
    )
     where

import Data.Maybe (isJust)
import Data.List (intersperse)

-- | Represents a word in the puzzle.
newtype Word' = Word' String

-- | Represents the characters that have been guessed in the puzzle.
newtype GuessedCharacters = GuessedCharacters [Maybe Char]

-- | Represents the characters that have been tried in the puzzle.
newtype TriedCharacters = TriedCharacters [Char]

-- | Represents the state of the puzzle.
data Puzzle = Puzzle Word' GuessedCharacters TriedCharacters

-- | Defines how to display a puzzle.
instance Show Puzzle where
  show (Puzzle _ (GuessedCharacters guessed) (TriedCharacters tries)) =
    "\nGuessed: " ++ intersperse ' ' (map displayGuessedChar guessed) ++
    "\nTried: " ++ intersperse ' ' tries
    where
      displayGuessedChar Nothing = '_'
      displayGuessedChar (Just c) = c

-- | Creates a fresh puzzle with the specified word.
freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle (Word' w) (GuessedCharacters (replicate (length w) Nothing)) (TriedCharacters [])

-- | Checks if a character is present in the puzzle's word.
charInPuzzle :: Puzzle -> Char -> Bool
charInPuzzle (Puzzle (Word' word) _ _) c = c `elem` word

-- | Checks if all characters in the guessed characters list are correct (not Nothing).
isAllCorrect :: GuessedCharacters -> Bool
isAllCorrect (GuessedCharacters guessed) = all isJust guessed

-- | Fills in the puzzle with the guessed character and updates the tried characters list.
fillInPuzzle :: Puzzle -> Char -> Puzzle
fillInPuzzle (Puzzle (Word' word) (GuessedCharacters filledInSoFar) (TriedCharacters tries)) c =
  Puzzle (Word' word) (GuessedCharacters newFilledInSoFar) (TriedCharacters (c : triedChars))
  where
    newFilledInSoFar =
      zipWith zipper word filledInSoFar
    zipper wordChar guessChar =
      if wordChar == c
        then Just c
        else guessChar
    triedChars = filter (/= c) tries

-- | Checks if a character has already been guessed in the puzzle.
alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ (GuessedCharacters guessed) _) c = Just c `elem` guessed

-- | Handles a guess in the puzzle, updating it accordingly.
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInPuzzle puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInPuzzle puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInPuzzle puzzle guess)

-- | Displays the word of the puzzle.
displayPuzzle'sWord :: Word' -> IO ()

displayPuzzle'sWord (Word' w) = putStrLn $ "The word was: " ++ w