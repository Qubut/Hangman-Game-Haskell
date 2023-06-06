# Hangman Game in Haskell

This is a command-line implementation of the classic game Hangman, where players guess letters to uncover a hidden word. The objective is to guess the word correctly before running out of attempts.
How to Play

1. Clone the repository to your local machine.

2. Install Haskell if you haven't already.

3. Open a terminal and navigate to the project directory.

4. Build the project by running the following command:

```sh
stack build
```
or 

```sh
    cabal build
```

Run the game using the following command:

```sh
    stack run hangman-exe
```
or

```sh
    cabal run hangman-exe
```

5. The game will display a series of underscores representing the hidden word and prompt you to guess a letter.

6. Enter a letter and press Enter to make a guess.

7. Continue guessing letters until you either guess the word correctly or run out of attempts.

8. If you guess the word correctly, you win the game. Otherwise, you lose.

## Project Structure

The project consists of the following modules:

- Main.hs: Contains the main entry point for the game.
- Hangman.hs: Provides functions for managing the hangman state and displaying hangman art.
- Puzzle.hs: Defines the data types and functions related to the game puzzle.
- WordsList.hs: Handles word selection and filtering.

## Dependencies

The project uses the following external dependencies:

- base: Provides basic Haskell functionality.
- random: Enables random word selection.
- split: Helps split words from the dictionary.
- ansi-terminal: Allows printing colored text in the terminal.

These dependencies are managed using the Cabal build system and will be automatically installed when building the project.

## License

This project is licensed under the BSD 3-Clause License. See the LICENSE file for more information.

## Contributing

Contributions to this project are welcome. Feel free to open issues for bug reports, feature requests, or general feedback. If you would like to contribute code, please submit a pull request with your changes.
Credits

This Hangman game was created by Qubut. It is based on the classic game and uses Haskell for implementation.
## Contact

For additional information or inquiries, please contact s-aahmed@haw-landshut.de.

*__Enjoy playing Hangman!__*
