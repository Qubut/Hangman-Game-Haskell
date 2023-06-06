module Hangman
    ( HangmanState(..)
    , savedHangmanArt
    , printHangman
    , printSavedHangman
    , initialHangmanState
    , updateHangmanState
    ) 
    where

-- | Represents the state of the hangman.
data HangmanState = HangmanState Int

-- | Generates the hangman art based on the current state.
generateHangmanArt :: Int -> String
generateHangmanArt n = unlines $ take n hangmanLines

-- | The lines of the hangman art.
hangmanLines :: [String]
hangmanLines =
  [ "   +---+",
    "   |   |",
    "   O   |",
    "  /|\\  |",
    "  / \\  |",
    "       |",
    "       |"
  ]

-- | The saved hangman art.
savedHangmanArt :: String
savedHangmanArt = unlines
  [ "   +---+",
    "   |   |",
    "       |",
    "       |",
    "       |",
    "   O   |",
    "  /|\\  |",
    "  / \\  |",
    "       |"
  ]

-- | Prints the hangman art based on the given state.
printHangman :: HangmanState -> IO ()
printHangman (HangmanState n) = do
  putStrLn $ generateHangmanArt n

-- | Prints the saved hangman art.
printSavedHangman :: IO ()
printSavedHangman = do
  putStrLn savedHangmanArt

-- | The initial hangman state.
initialHangmanState :: HangmanState
initialHangmanState = HangmanState 0

-- | Updates the hangman state.
updateHangmanState :: HangmanState -> HangmanState
updateHangmanState (HangmanState n) = HangmanState (n + 1)
