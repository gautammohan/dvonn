# Dvonn

This is an implementation of the board game [DVONN][https://boardgamegeek.com/boardgame/2346/dvonn]. Instructions for how to play are provided on the splash screen. This project uses stack, and you can play using `stack run`.

# Module structure

The ordered overview of the
files is as follows:

- *Defs.hs*: Top-level file containing the type declarations for this project.

- *Board.hs*: generic functions that compute properties of and operate on boards.

- *Move.hs*: pure functions that validate and parse moves on boards

- *Game.hs*: A file defining the game monad as well as sequences of computations
that determine the control flow of the game

- *IOInterface.hs*: A module for printing boards to the screen

- *Test/Spec.hs*: Unit tests and quickcheck properties validating the game

- *main/Main.hs*: The main file from which dvonn games are played

- *main/Demo.hs*: Intermediate files that can be used to play the game from
intermediate states

# Authors

Written by Emily Diana and Gautam Mohan. Original board game created by Kris Burm.
