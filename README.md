# fpse-chess
 Final project for 601.629 FPSE

## Module Decriptions
We implemented a chess game with chess bot opponent. Below are the high-level functionalities included in each module:
 - Board.ml: Includes definitions of board, chesspieces, and players. Also includes functions for performing move and castling, which are the two major functions used by other modules.
 - Bot.ml: Includes definition of Evaluator and Bot. Evaluators' main responsibility is scoring boards from the given player's perspective. Bots' takes a evaluator as functor, and implement their own algorithms based on the scores provided by Evaluators. Bot.ml includes a bot implementation based on Minimax algorithm.
 - Controller.ml: Controller of the game. Takes user commands, executes moves on board, then uses Gui.ml to print results.
 - Gui.ml: Provides GUI funcions.

## Building and Running
Simply run `dune exec -- ./src/controller.exe` to build and run the project. `dune exec -- ./src/controller.exe --help` shows instructions on how to start playing:
````
[--bot string]       play against a bot, takes string "white" or "black"
  [--castling string]  castling, takes string "queen" or "king" for castling
                       towards queen or king side
  [--init]             initialize game
  [--load string]      game to load
  [--move string]      move piece, takes string in the form "0,1 0,3"
  [--save string]      saved name
  [-build-info]        print info about this build and exit
  [-version]           print the version of this build and exit
  [-help]              print this help text and exit
                       (alias: -?)
````
