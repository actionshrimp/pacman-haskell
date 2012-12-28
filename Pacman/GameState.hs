module Pacman.GameState (GameStateId, GameStateIdT(..), GameState(..)) where

import Pacman.Actor

data GameStateIdT a = Starting | PacmanDead | GhostActivating a | GhostReturningHome a | GhostsFleeing

type GameStateId = GameStateIdT GhostId

data GameState = GameState {
    gameStateId :: GameStateId,
    gameStateRemainingTime :: Float
}
