module WordsList
  ( gameWords
  , randomWord
  , minWordLength
  , maxWordLength
  , extraTries
  ) where

import System.Random (randomRIO)

-- | Represents a list of words.
type WordList = [String]

-- | Reads all the words from a file.
allWords :: IO WordList
allWords = lines <$> readFile "data/dict.txt"

-- | The maximum length of a word allowed in the game.
maxWordLength :: Int
maxWordLength = 9

-- | The minimum length of a word allowed in the game.
minWordLength :: Int
minWordLength = 5

-- | The number of extra tries granted in the game.
extraTries :: Int
extraTries = 3

-- | Filters the word list to contain words within the allowed length range.
gameWords :: IO WordList
gameWords = filter isAllowed <$> allWords
  where
    isAllowed w = let l = length w in l >= minWordLength && l <= maxWordLength

-- | Retrieves a random word from the word list.
getRandomWord :: WordList -> IO String
getRandomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

-- | Generates a random word for the game.
randomWord :: IO String
randomWord = getRandomWord =<< gameWords
