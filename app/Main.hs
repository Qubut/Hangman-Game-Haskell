module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust, mapMaybe)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

type WordList = [String]

newtype Word' = Word' String

newtype GuessedCharacters = GuessedCharacters [Maybe Char]

newtype TriedCharacters = TriedCharacters [Char]

data Puzzle = Puzzle Word' GuessedCharacters TriedCharacters

instance Show Puzzle where
  show (Puzzle _ (GuessedCharacters guessed) (TriedCharacters tries)) =
    "\nGuessed: " ++ intersperse ' ' (map displayGuessedChar guessed) ++
    "\nTried: " ++ intersperse ' ' tries
    where
      displayGuessedChar Nothing = '_'
      displayGuessedChar (Just c) = c

allWords :: IO WordList
allWords = lines <$> readFile "data/dict.txt"

maxWordLength :: Int
maxWordLength = 9

minWordLength :: Int
minWordLength = 5

gameWords :: IO WordList
gameWords = filter isAllowed <$> allWords
  where
    isAllowed w = let l = length w in l >= minWordLength && l <= maxWordLength

getRandomWord' :: WordList -> IO String
getRandomWord' wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord :: IO String
randomWord = getRandomWord' =<< gameWords

freshPuzzle :: String -> Puzzle
freshPuzzle w = Puzzle (Word' w) (GuessedCharacters (replicate (length w) Nothing)) (TriedCharacters [])


charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle (Word' word) _ _) c = c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle (Word' word) (GuessedCharacters guessed) _) c = Just c `elem` guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle (Word' word) (GuessedCharacters filledInSoFar) (TriedCharacters tries)) c =
  Puzzle (Word' word) (GuessedCharacters newFilledInSoFar) (TriedCharacters (c : triedChars))
  where
    newFilledInSoFar =
      zipWith zipper word filledInSoFar
    zipper wordChar guessChar =
      if wordChar == c
        then Just c
        else guessChar
    triedChars = filter (/= c) tries

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

displayWord :: Word' -> IO ()
displayWord (Word' w) = putStrLn $ "The word was: " ++ w

isAllCorrect :: GuessedCharacters -> Bool
isAllCorrect (GuessedCharacters guessed) = all isJust guessed

gameOver :: Puzzle -> IO ()
gameOver (Puzzle (Word' word) guessed (TriedCharacters tries)) =
  if length tries > length word + 3 && not (isAllCorrect guessed) then do
    putStrLn "You lose!"
    displayWord (Word' word)
    exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle (Word' word) guessed _) =
  if isAllCorrect guessed then do
    putStrLn "You win!"
    displayWord (Word' word)
    exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  word <- randomWord
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
