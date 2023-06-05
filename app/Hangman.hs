module Hangman where

data HangmanState = HangmanState Int

generateHangmanArt :: Int -> String
generateHangmanArt n = unlines $ take n hangmanLines

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

printHangman :: HangmanState -> IO ()
printHangman (HangmanState n) = do
  putStrLn $ generateHangmanArt n

printSavedHangman :: IO ()
printSavedHangman = do
  putStrLn savedHangmanArt

initialHangmanState :: HangmanState
initialHangmanState = HangmanState 0

updateHangmanState :: HangmanState -> HangmanState
updateHangmanState (HangmanState n) = HangmanState (n + 1)
